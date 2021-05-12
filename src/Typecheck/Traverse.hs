{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Hash Compiler type checking AST traversal and Hindley-Milner application.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Typecheck.Traverse where

import Control.Applicative ((<|>))
import Control.Error (catMaybes, fromMaybe, maybeToList, runExceptT)
import Control.Lens (imapM, over, view, views, (.~), (?~))
import Control.Monad (foldM, unless, when)
import Control.Monad.State (MonadIO (liftIO), gets, modify)
import Control.Monad.Trans (lift)
import Data.Bifunctor (Bifunctor (first), second)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Text (pack)
import qualified Data.Vector as V
import Panic
import qualified Parse.Ast as A
import qualified Parse.Boot as PB
import qualified Runtime.Boot as RB
import Runtime.Intrinsics (intrinsicMap)
import qualified Runtime.Primitives as RP
import System.FilePath (takeDirectory)
import Typecheck.Boot
import Typecheck.Infer
import Utils (zipMaybe)
import qualified Utils as U

-- | Type class to transform an AST node into its type, any substitutions that
-- were created, and any emitted instructions.
class TypedNode a where
  getNodeType :: PB.AstNode a -> Typecheck (Type, Substitution, Maybe RB.Instruction)

-- | Any AST node surrounded by `PB.AstNode` should also implement `TypedNode`.
--
-- This is merely for convenience.
instance TypedNode a => TypedNode (PB.AstNode a) where
  getNodeType = getNodeType . PB.body

-- | Create a new generated type variable with the given bound, and add it to
-- the global table.
newTypeVarWithBound :: PB.Name -> Bound -> Typecheck Type
newTypeVarWithBound name bound = do
  -- new type var
  GenTypeVar v <- newTypeVar

  -- Create a genTypeVar context so that we replace type variables with
  -- their generated versions.
  genTypeVarCtx <- HM.insert name (GenTypeVar v) <$> createGenTypeVarCtx (view boundVars bound)

  -- Do the same with trait bounds.
  let instantiatedArgsBoundTraits = replaceTypeVarsInBoundTraits genTypeVarCtx (view boundTraits bound)

  -- Set the trait bounds of the type to the state.
  addGenTypeVarBounds v instantiatedArgsBoundTraits

  -- Return the defined type.
  return $ GenTypeVar v

-- | Check that the given nodes are of the same type.
--
-- If so, return the type, as well as any substitutions.
ensureSameType :: (TypedNode a) => PB.Name -> Bound -> [PB.AstNode a] -> Typecheck (Type, Substitution, [RB.Instruction])
ensureSameType n b [] = (,noSub,[]) <$> newTypeVarWithBound n b
ensureSameType n b (e : entries) = do
  (firstT, firstS, firstI) <- getNodeType e
  (restT, restS, restI) <- ensureSameType n b entries
  let allS = mergeSubs [firstS, restS]
  let subbedT = applySub allS firstT
  (resT, resS) <- unifyTypes e restT subbedT
  return (resT, resS, maybeToList firstI ++ restI)

-- | Convert an AST type annotation into a `Type`.
convertAstTypeToType :: PB.AstNode PB.Type -> Typecheck Type
convertAstTypeToType t = do
  (args, binds) <- convertAstTypeToTypeWithBinds t
  if HS.null binds
    then return args
    else do
      -- Lookup the names so that we fail
      mapM_ lookupName (HS.toList binds)
      unreachablePure

-- | Convert an AST type annotation into a `Type`.
--
-- This doesn't try to resolve the types, just parses type variables.
convertAstTypeToTypeJustBinds :: PB.AstNode PB.Type -> Typecheck (HS.HashSet (PB.AstNode PB.Name))
convertAstTypeToTypeJustBinds node = case PB.body node of
  (PB.TypeVar n) ->
    maybeLookupName n >>= \case
      Just (TypeSymbol _) -> return HS.empty
      Nothing -> return (HS.singleton n)
      _ -> typeError node $ "Symbol '" ++ show n ++ "' is not a type."
  PB.ExistentialType -> return HS.empty
  PB.InferType -> return HS.empty
  (PB.NamedType (PB.body -> PB.AccessName [PB.body -> PB.Name "Tuple"]) args) ->
    HS.unions <$> mapM convertAstTypeToTypeJustBinds args
  (PB.NamedType accessName []) -> do
    lookupSymbol accessName >>= \case
      TypeSymbol _ -> return HS.empty
      _ -> typeError accessName $ "Symbol '" ++ show accessName ++ "' is not a type."
  (PB.NamedType accessName args) -> do
    lookupSymbol accessName >>= \case
      TypeConsSymbol _ -> HS.unions <$> mapM convertAstTypeToTypeJustBinds args
      _ -> typeError accessName $ "Symbol '" ++ show accessName ++ "' is not a type constructor."
  (PB.FunctionType args ret) -> do
    bindsArgs <- mapM convertAstTypeToTypeJustBinds args
    bindsRet <- convertAstTypeToTypeJustBinds ret
    return $ HS.unions (bindsRet : bindsArgs)

-- | Convert an AST type annotation into a `Type`.
--
-- This also resolves the given types.
convertAstTypeToTypeWithBinds :: PB.AstNode PB.Type -> Typecheck (Type, HS.HashSet (PB.AstNode PB.Name))
convertAstTypeToTypeWithBinds node = case PB.body node of
  (PB.TypeVar n) -> do
    ctx <- gets . view $ typeVarCtx
    case PB.body n `HM.lookup` ctx of
      Just ty -> return (ty, HS.empty)
      _ ->
        maybeLookupName n >>= \case
          Just (TypeSymbol t) -> return (t, HS.empty)
          Nothing -> return (TypeVar (PB.body n), HS.singleton n)
          _ -> typeError node $ "Symbol '" ++ show n ++ "' is not a type."
  PB.ExistentialType -> return (ExistentialT, HS.empty)
  PB.InferType -> (,HS.empty) <$> newTypeVar
  (PB.NamedType (PB.body -> PB.AccessName [PB.body -> PB.Name "Tuple"]) args) -> do
    -- Special handling for tuple, this probably needs to be changed from within parser.
    (convArgs, binds) <- unzip <$> mapM convertAstTypeToTypeWithBinds args
    return (TupleT convArgs, HS.unions binds)
  (PB.NamedType accessName []) -> do
    lookupSymbol accessName >>= \case
      TypeSymbol t -> return (t, HS.empty) -- @Todo: add type constructor inferring args.
      _ -> typeError accessName $ "Symbol '" ++ show accessName ++ "' is not a type."
  (PB.NamedType accessName args) -> do
    lookupSymbol accessName >>= \case
      TypeConsSymbol tId -> do
        -- Get the bound of the type
        tBound <- typeDefBound <$> lookupTypeDef tId

        -- Create a genTypeVar context so that we replace type variables with
        -- their generated versions.
        genTypeVarCtx <- createGenTypeVarCtx (view boundVars tBound)

        -- Create the version of the type arguments that replaces type
        -- variables with the generated type variables above.
        let instantiatedArgs =
              map
                ( \v ->
                    fromMaybe (internalPanicPure "expected to find type variable in ctx") $
                      HM.lookup v genTypeVarCtx
                )
                (view boundVars tBound)

        -- Do the same with trait bounds.
        let instantiatedArgsBoundTraits = replaceTypeVarsInBoundTraits genTypeVarCtx (view boundTraits tBound)

        -- Set the trait bounds of the type to the state.
        -- @Improvement: We set the same bound for each of the generated types,
        -- this could be more efficient.
        mapM_
          ( \case
              GenTypeVar g -> addGenTypeVarBounds g instantiatedArgsBoundTraits
              _ -> internalPanicPure "expected generated type variables in genTypeVarCtx"
          )
          (HM.elems genTypeVarCtx)

        -- Convert the arguments from AST types to typechecking types.
        (convArgs, binds) <- unzip <$> mapM convertAstTypeToTypeWithBinds args

        -- Unify type
        (unifyT, _) <- unifyTypes node (DefinedType tId instantiatedArgs) (DefinedType tId convArgs)

        -- Return the unified type.
        return (unifyT, HS.unions binds)
      _ -> typeError accessName $ "Symbol '" ++ show accessName ++ "' is not a type constructor."
  (PB.FunctionType args ret) -> do
    (convArgs, bindsArgs) <- unzip <$> mapM convertAstTypeToTypeWithBinds args
    (convRet, bindsRet) <- convertAstTypeToTypeWithBinds ret
    return (FunctionT $ FunctionType convArgs convRet [], HS.unions (bindsRet : bindsArgs))

-- | Enter a function (modifies typechecker state).
enterFunction :: Type -> Typecheck a -> Typecheck a
enterFunction t f = do
  oldF <- gets . view $ funcRetType
  oldL <- gets . view $ inLoop
  oldRO <- gets . view $ retOnce
  modify $ funcRetType ?~ t
  modify $ retOnce .~ False
  modify $ inLoop .~ False
  x <- f
  modify $ funcRetType .~ oldF
  modify $ retOnce .~ oldRO
  modify $ inLoop .~ oldL
  return x

-- | Enter a loop (modifies typechecker state).
enterLoop :: Typecheck a -> Typecheck a
enterLoop f = do
  old <- gets . view $ inLoop
  modify $ inLoop .~ True
  x <- f
  modify $ inLoop .~ old
  return x

-- | Make sure typechecker is currently in a function, and if so unify with the
-- given return type.
ensureInFunction :: PB.AstNode a -> Type -> String -> Typecheck (Type, Substitution)
ensureInFunction n t msg = do
  currRetType <- gets . view $ funcRetType
  case currRetType of
    Just ret -> do
      (resT, resS) <- unifyTypes n ret t
      modify $ funcRetType ?~ resT
      return (resT, resS)
    Nothing -> typeError n msg

-- | Make sure typechecker is currently in a loop.
ensureInLoop :: PB.AstNode a -> String -> Typecheck ()
ensureInLoop n msg = do
  curr <- gets . view $ inLoop
  unless curr $ typeError n msg

-- | Match a `TypeDef` with a pattern.
--
-- Returns (substitition, hasBinds, patternToEmit)
ensureTypeDefPatternMatches ::
  TypeId ->
  [Type] ->
  PB.AstNode PB.Pattern ->
  TypeDef ->
  Typecheck
    ( Substitution,
      Bool,
      RB.PatternR
    )
ensureTypeDefPatternMatches tId tArgs a@(PB.body -> p) tDef = case (tDef, p) of
  (StructT structTyp, PB.PatternStruct name entries) -> do
    -- Evaluate the patterns of each field.
    (subs, binds, pats) <-
      unzip3
        <$> mapM
          ( \(PB.body -> PB.DestructuringEntry dName dPat) -> do
              case PB.body dName `HM.lookup` view structFields structTyp of
                Just (StructField fieldT _ _) -> do
                  modify $ over symbolScopes newSymbolTable
                  givenTypeArgs <- mapM (const newTypeVar) (view (structBound . boundVars) structTyp)
                  (_, _, ctx) <- matchesBound name givenTypeArgs (view structBound structTyp)
                  modify $ over symbolScopes popSymbolTable

                  ensurePatternMatches (replaceTypeVars ctx fieldT) dPat
                Nothing -> do
                  sShow <- showTypeDef tArgs tDef
                  typeError dName $ "Struct type '" ++ sShow ++ "' does not have member '" ++ show dName ++ "'."
          )
          entries
    return (mergeSubs subs, or binds, RB.StructP pats)
  -- Sometimes enums are parsed as `PB.PatternBinding` because the resolution
  -- is ambiguous at parsing time.
  (EnumT EnumType {}, PB.PatternBinding n) -> do
    -- Get ast info from the current one and just make a new node
    let node = A.astNodeAt a (PB.PatternEnum (A.astNodeAt n (PB.AccessName [n])) [])
    ensureTypeDefPatternMatches tId tArgs node tDef
  (EnumT enumTyp, PB.PatternEnum name ps) -> do
    enumSym <- lookupSymbol name
    case enumSym of
      EnumVariantSymbol enumIdSym enumVariantName
        | enumIdSym == tId -> do
          modify $ over symbolScopes newSymbolTable
          let (variantArgs, variantId) =
                fromMaybe
                  (internalPanicPure "expected to find name in enum")
                  (enumVariantName `HM.lookup` view enumVariants enumTyp)

          givenTypeArgs <- mapM (const newTypeVar) (view (enumBound . boundVars) enumTyp)
          (_, _, ctx) <- matchesBound name givenTypeArgs (view enumBound enumTyp)
          let expectedArgs = map (replaceTypeVars ctx) variantArgs
          modify $ over symbolScopes popSymbolTable
          (sub, binds, pats) <- ensurePatternsMatch "Enum variant mismatch in length." name expectedArgs ps

          return (sub, binds, RB.EnumP variantId pats)
      _ -> do
        eShow <- showTypeDef tArgs tDef
        typeError name $ "Symbol '" ++ show name ++ "' is not a variant of enum type '" ++ eShow ++ "'."
  (NamespaceT nName syms, PB.PatternNamespace entries) -> do
    -- Evaluate patterns of each field of namespace.
    (subs, binds, pats) <-
      unzip3
        <$> mapM
          ( \(PB.body -> PB.DestructuringEntry dName dPat) -> do
              case PB.body dName `HM.lookup` view symbolData syms of
                Just sym -> do
                  case sym of
                    ValueSymbol ref ty -> do
                      let ins = RB.INameRef (RB.Name ref)
                      (sub, bind, pat) <- ensurePatternMatches ty dPat
                      return (sub, bind, RB.IPatBind pat ins)
                    _ -> case PB.body dPat of
                      PB.PatternBinding bn -> do
                        addSymbol (PB.body bn) sym
                        return (noSub, True, RB.ILiteral RB.VoidV)
                      _ -> typeError dPat $ "Cannot pattern match on non-value symbol '" ++ show dName ++ "'."
                Nothing -> do
                  typeError dName $ "Namespace '" ++ nName ++ "' does not export symbol '" ++ show dName ++ "'."
          )
          entries
    -- Hack to get runtime to work:
    -- Convert pattern to "ignore, if x", where x is a list of `RB.IPatBind`s.
    return (mergeSubs subs, or binds, RB.CondP RB.IgnoreP (RB.ISeq $ pats ++ [RB.ILiteral RP.boolTrueV]))
  _ -> typeError a "Invalid pattern."

-- | Match a `Type` with a pattern.
--
-- Returns (substitition, hasBinds, patternToEmit)
ensurePatternMatches :: Type -> PB.AstNode PB.Pattern -> Typecheck (Substitution, Bool, RB.PatternR)
ensurePatternMatches t p = case (t, PB.body p) of
  (_, PB.PatternIgnore) -> return (noSub, False, RB.IgnoreP)
  (_, PB.PatternIf p e) -> do
    (patS, hasBind, patI) <- ensurePatternMatches t p
    (exprT, exprS, condI) <- getNodeType e
    (_, unifyS) <- unifyTypes e (DefinedType (enumPrimitiveId BoolT) []) exprT
    return
      ( mergeSubs [patS, exprS, unifyS],
        hasBind,
        RB.CondP
          patI
          (fromMaybe (internalPanicPure "Expected instruction") condI)
      )
  (_, PB.PatternOr a b) -> do
    (aS, aHasBind, aP) <- ensurePatternMatches t a
    (bS, bHasBind, bP) <- ensurePatternMatches t b
    case (aHasBind, bHasBind) of
      (False, False) -> do
        return (mergeSubs [aS, bS], False, RB.OrP [aP, bP])
      (True, _) -> bindError a
      (_, True) -> bindError b
    where
      bindError x = typeError x "Cannot have bindings in compound pattern."
  (t, PB.PatternBinding n) -> do
    let valHandle = do
          (ValueSymbol id _) <- addValueName (PB.body n) t
          return (noSub, True, RB.BindP (RB.Name id))
    case t of
      DefinedType tId [] ->
        lookupTypeDef tId >>= \case
          NamespaceT {} -> do
            _ <- addTypeSymbol (PB.body n) t
            return (noSub, True, RB.IgnoreP)
          EnumT ty ->
            maybeLookupName n >>= \case
              Just EnumVariantSymbol {} -> do
                let node = A.astNodeAt n (PB.PatternEnum (A.astNodeAt n (PB.AccessName [n])) [])
                ensureTypeDefPatternMatches tId [] node (EnumT ty)
              _ -> valHandle
          _ -> valHandle
      _ -> valHandle
  (t, PB.PatternLiteral (PB.body -> (PB.CharLiteral c)))
    | t == primitiveTy CharT [] ->
      return
        ( noSub,
          False,
          RB.CharP c
        )
  (t, PB.PatternLiteral (PB.body -> (PB.StrLiteral s)))
    | t == primitiveTy StrT [] ->
      return
        ( noSub,
          False,
          RB.StrP (pack s)
        )
  (t, PB.PatternLiteral (PB.body -> (PB.IntLiteral i)))
    | t == primitiveTy IntT [] ->
      return
        ( noSub,
          False,
          RB.IntP i
        )
  (t, PB.PatternLiteral (PB.body -> (PB.FloatLiteral f)))
    | t == primitiveTy FloatT [] ->
      return
        ( noSub,
          False,
          RB.FloatP f
        )
  (TupleT ts, PB.PatternTuple ps) -> do
    (sub, binds, pats) <- ensurePatternsMatch "Mismatching tuple pattern arguments." p ts ps
    return (sub, binds, RB.TupleP pats)
  (DefinedType tId tArgs, _) -> lookupTypeDef tId >>= ensureTypeDefPatternMatches tId tArgs p
  _ -> typeError p "Invalid pattern."

-- | Match multiple patterns with multiple types.
--
-- Returns (substitition, hasBinds, patternsToEmit)
ensurePatternsMatch :: String -> PB.AstNode a -> [Type] -> [PB.AstNode PB.Pattern] -> Typecheck (Substitution, Bool, [RB.PatternR])
ensurePatternsMatch msg n ts ps =
  foldM
    ( \(sub, hasBind, pats) (mt, mp) -> do
        case (mt, mp) of
          (Just t, Just p) -> do
            (newSub, newHasBind, pat) <- ensurePatternMatches t p
            return (mergeSubs [sub, newSub], hasBind || newHasBind, pats ++ [pat])
          _ -> typeError n $ msg ++ " Expected " ++ show (length ts) ++ " but got " ++ show (length ps) ++ "."
    )
    (noSub, False, [])
    (zipMaybe ts ps)

-- | Ensure that the given AST entries are consistent with a map type.
--
-- Returns (substitition, hasBinds, patternsToEmit)
ensureMapType :: (TypedNode k, TypedNode v) => [(PB.AstNode k, PB.AstNode v)] -> Typecheck ((Type, Type), Substitution, [(RB.Instruction, RB.Instruction)])
ensureMapType [] = do
  k <- newTypeVarWithBound (PB.Name "K") (typeDefBound $ primitiveT MapT)
  v <- newTypeVarWithBound (PB.Name "V") (typeDefBound $ primitiveT MapT)
  return ((k, v), noSub, [])
ensureMapType ((k, v) : entries) = do
  (firstKT, firstKS, firstKI) <- getNodeType k
  (firstVT, firstVS, firstVI) <- getNodeType v

  let firstI = case (firstKI, firstVI) of
        (Just a, Just b) -> (a, b)
        _ -> internalPanicPure "Expected instructions for map literal"

  ((restKT, restVT), restS, restI) <- ensureMapType entries

  let allS = mergeSubs [firstKS, firstVS, restS]
  let subbedKT = applySub allS firstKT
  let subbedVT = applySub allS firstVT

  (unifyKT, unifyKS) <- unifyTypes k restKT subbedKT
  (unifyVT, unifyVS) <- unifyTypes v restVT subbedVT
  return ((unifyKT, unifyVT), mergeSubs [unifyKS, unifyVS], firstI : restI)

-- | Convert an AST forall type into a Bound.
convertForAll :: PB.AstNode PB.ForAll -> Typecheck Bound
convertForAll f = do
  ImplBound tNames tArgs traits <- convertForAllWithBinds f
  nameArgs <-
    mapM
      ( \case
          (TypeVar t) ->
            if t `HS.member` tNames
              then return t
              else typeError f $ "Type argument '" ++ show t ++ "' already in use"
          a -> do
            typ <- showType a
            typeError f $ "Type arguments must only be generic arguments, not '" ++ typ ++ "'"
      )
      tArgs
  return $ Bound nameArgs traits

-- | Convert an AST forall type into a Bound.
--
-- Also extract all type bindings from complex types (used for trait
-- implementations).
convertForAllWithBinds :: PB.AstNode PB.ForAll -> Typecheck ImplBound
convertForAllWithBinds (PB.body -> (PB.ForAll tArgs tBounds)) = do
  -- map over the type args and make sure they are all TypeVars, otherwise
  -- we raise a typecheck error since it doesn't make sense
  cNames <- HS.unions <$> mapM convertAstTypeToTypeJustBinds tArgs

  -- Add type symbols to scope
  mapM_ (\(PB.body -> b) -> addTypeSymbol b (TypeVar b)) (HS.toList cNames)

  -- Parse all bounds
  cBounds <-
    BoundTraits
      <$> mapM
        ( \(PB.body -> PB.TraitBound traitName traitTypes) -> do
            lookupSymbol traitName >>= \case
              TraitSymbol traitId -> do
                convTypes <- mapM convertAstTypeToType traitTypes

                tDef <- lookupTraitDef traitId

                (_, sub1, ctx) <- matchesBound traitName convTypes (view traitBound tDef)
                let FunctionT fnType = applySub sub1 $ replaceTypeVars ctx $ FunctionT (view traitType tDef)

                -- Add parsed bounds
                addTypeVarBounds $ BoundTraits [(TraitBound traitId convTypes, fnType)]

                return (TraitBound traitId convTypes, fnType)
              _ -> typeError traitName $ "Symbol '" ++ show traitName ++ "' is not a trait."
        )
        tBounds

  -- Now parse again but with type resolution
  (cArgs, _) <- second HS.unions . unzip <$> mapM convertAstTypeToTypeWithBinds tArgs
  return $ ImplBound (HS.map PB.body cNames) cArgs cBounds

-- | Get the types of a sequence of nodes.
getNodeTypes :: TypedNode a => [PB.AstNode a] -> Typecheck ([Type], Substitution, [RB.Instruction])
getNodeTypes xs = do
  (types, subs, ins) <- unzip3 <$> mapM getNodeType xs
  return (types, mergeSubs subs, catMaybes ins)

-- | TypedNode for REPLBlock
instance TypedNode PB.REPLBlock where
  getNodeType n = case PB.body n of
    PB.REPLBlock statements Nothing -> do
      (_, sub, ins) <- getNodeTypes statements
      return (primitiveTy VoidT [], sub, Just $ RB.ISeq ins)
    PB.REPLBlock statements (Just expr) -> do
      (_, sub, ins) <- getNodeTypes statements
      (exprT, exprS, insLast) <- getNodeType expr
      return (exprT, mergeSubs [exprS, sub], Just $ RB.ISeq (ins ++ maybeToList insLast))

-- | TypedNode for Block
instance TypedNode PB.Block where
  getNodeType n = case PB.body n of
    PB.Match expr cases -> do
      (exprT, exprS, exprI) <- getNodeType expr
      exprTypeVar <- newTypeVar
      modify $ over symbolScopes newSymbolTable
      -- Evaluate each case of the match.
      (ty, sub, pats) <-
        foldM
          ( \(unifyingT, unifyingS, unifyingI) matchCase -> do
              let (PB.MatchCase cPat cExpr) = PB.body matchCase
              (patS, _, patP) <- ensurePatternMatches exprT cPat
              (caseT, caseS, caseI) <- getNodeType cExpr
              let mergedSubs = mergeSubs [patS, caseS, unifyingS]
              (unifiedT, unifiedS) <- unifyTypes cExpr unifyingT caseT

              let patIns = (patP,) <$> caseI
              return
                ( applySub mergedSubs unifiedT,
                  mergeSubs [mergedSubs, unifiedS],
                  unifyingI ++ maybeToList patIns
                )
          )
          (exprTypeVar, exprS, [])
          cases
      modify $ over symbolScopes popSymbolTable
      let ins = RB.IMatch (fromMaybe (internalPanicPure "Expected instruction for match expr") exprI) pats
      return (ty, sub, Just ins)
    PB.Loop inner -> enterLoop $ do
      modify $ over symbolScopes newSymbolTable
      (innerT, innerS, unifyI) <- getNodeType inner
      (_, unifyS) <- unifyTypes inner (primitiveTy VoidT []) innerT
      modify $ over symbolScopes popSymbolTable
      return
        ( primitiveTy VoidT [],
          mergeSubs [innerS, unifyS],
          Just . RB.ILoop $ fromMaybe (internalPanicPure "Expected instruction for loop") unifyI
        )
    PB.Body statements Nothing -> do
      modify $ over symbolScopes newSymbolTable
      (_, sub, ins) <- getNodeTypes statements
      modify $ over symbolScopes popSymbolTable
      return (primitiveTy VoidT [], sub, Just . RB.IBlock . RB.ISeq $ ins)
    PB.Body statements (Just expr) -> do
      modify $ over symbolScopes newSymbolTable
      (_, sub, ins) <- getNodeTypes statements
      (exprT, exprS, exprIns) <- getNodeType expr
      modify $ over symbolScopes popSymbolTable
      return (exprT, mergeSubs [exprS, sub], Just . RB.IBlock . RB.ISeq $ ins ++ maybeToList exprIns)

-- | TypedNode for Statement
instance TypedNode PB.Statement where
  getNodeType n = case PB.body n of
    PB.ExprStatement expr -> do
      (_, sub, ins) <- getNodeType expr
      return (primitiveTy VoidT [], sub, ins)
    PB.Return expr -> do
      (retT, retS, retI) <- maybe (return (primitiveTy VoidT [], noSub, Just $ RB.ILiteral RB.VoidV)) getNodeType expr
      (_, unifyS) <- ensureInFunction n retT "The 'return' keyword can only be used inside a function definition."
      modify $ retOnce .~ True
      -- Send return signal
      return
        ( primitiveTy VoidT [],
          mergeSubs [retS, unifyS],
          Just . RB.IReturn $ fromMaybe (internalPanicPure "expected instructon in return") retI
        )
    PB.BlockStatement block -> do
      (blockT, blockS, blockI) <- getNodeType block
      (_, unifyS) <- unifyTypes block (primitiveTy VoidT []) blockT
      return (primitiveTy VoidT [], mergeSubs [blockS, unifyS], blockI)
    PB.Break -> do
      ensureInLoop n "The 'break' keyword can only be used inside a loop."
      return (primitiveTy VoidT [], noSub, Just RB.IBreak)
    PB.Continue -> do
      ensureInLoop n "The 'continue' keyword can only be used inside a loop."
      return (primitiveTy VoidT [], noSub, Just RB.IContinue)
    PB.StructDef (PB.body -> PB.Struct astName@(PB.body -> name) sForAll sEntries) -> do
      ensureNotDefined astName -- Make sure not to redefine.
      modify $ over symbolScopes newSymbolTable
      convBound <- convertForAll sForAll

      -- For each given field, typecheck and create field entries:
      (sFields, subs) <-
        unzip
          <$> imapM
            ( \i (PB.body -> PB.StructEntry astEName@(PB.body -> eName) eTy eDefault) -> do
                case (eTy, eDefault) of
                  (Nothing, Nothing) -> typeError astEName "Missing type annotation or default value."
                  (_, Just _) -> typeError astEName "Default struct fields are not supported at the moment."
                  _ -> return ()

                convTy <- maybe newTypeVar convertAstTypeToType eTy
                (resTy, resSub, defaultI) <- maybe ((,noSub,Nothing) <$> newTypeVar) getNodeType eDefault

                -- Looks awful but this just gets whichever AST node exists.
                let typePos = (\x -> x {PB.body = ()}) <$> eTy
                let defaultArgPos = (\x -> x {PB.body = ()}) <$> eDefault
                -- We already checked that one of them exists.
                let typeErrorNode = fromMaybe unreachablePure (typePos <|> defaultArgPos)

                (unifyTy, unifySub) <- unifyTypes typeErrorNode (applySub resSub convTy) resTy

                let field =
                      StructField
                        { _structFieldType = unifyTy,
                          _structFieldIndex = i,
                          _structFieldDefault = defaultI -- TODO: broken
                        }
                return ((eName, field), mergeSubs [unifySub, resSub]) -- @Speed: do we really need to unify here?
            )
            sEntries

      -- create the struct type and then insert it into the type table
      tId <-
        addType
          ( StructT $
              StructType
                { _structName = name,
                  _structBound = convBound,
                  _structFields = HM.fromList sFields
                }
          )
      modify $ over symbolScopes popSymbolTable

      _ <-
        if convBound == emptyBound
          then addTypeSymbol name (DefinedType tId [])
          else addTypeConsSymbol name tId

      return (primitiveTy VoidT [], mergeSubs subs, Nothing)
    PB.EnumDef (PB.body -> PB.Enumeration astName@(PB.body -> name) eForAll eEntries) -> do
      ensureNotDefined astName -- Make sure not to redefine.
      modify $ over symbolScopes newSymbolTable
      convBound <- convertForAll eForAll

      -- map over all the variants and apply some type substitution magic
      eVariants <-
        mapM
          ( \((PB.body -> (PB.EnumEntry (PB.body -> eName) eTypes))) -> do
              typ <- mapM convertAstTypeToType eTypes
              eId <- newRuntimeEnumVariant

              -- insert the variant into the symbol table here
              return (eName, (typ, eId))
          )
          eEntries

      -- create the enum type and then insert it into the type table
      tId <-
        addType
          ( EnumT $
              EnumType
                { _enumName = name,
                  _enumBound = convBound,
                  _enumVariants = HM.fromList eVariants
                }
          )
      modify $ over symbolScopes popSymbolTable

      -- Add enum type to scope
      _ <-
        if convBound == emptyBound
          then addTypeSymbol name (DefinedType tId [])
          else addTypeConsSymbol name tId

      -- Add variants to scope
      mapM_ ((`addEnumVarSymbol` tId) . fst) eVariants

      return (primitiveTy VoidT [], noSub, Nothing)
    PB.TraitDef (PB.body -> PB.Trait tName tForAll ty) -> do
      ensureNotDefined tName -- Make sure not to redefine.
      modify $ over symbolScopes newSymbolTable
      -- Convert trait forAll type.
      convBound <- convertForAll tForAll
      traitTy <- convertAstTypeToType ty
      modify $ over symbolScopes popSymbolTable
      -- Create the definition.
      case traitTy of
        FunctionT funcTy -> do
          let traitDef =
                Trait
                  { _traitName = PB.body tName,
                    _traitBound = convBound,
                    _traitType = funcTy,
                    _traitImpls = []
                  }
          tId <- addTrait traitDef -- Add trait to trait table
          _ <- addTraitSymbol (PB.body tName) tId -- Add trait to symbols
          return (primitiveTy VoidT [], noSub, Nothing)
        _ -> typeError ty "Trait must be a function type."
    PB.Let pat maybeForAll maybeType maybeExpr -> do
      prevFnName <- gets . view $ funcName

      -- Set fn name (for stack trace)
      case (PB.body pat, maybeExpr) of
        (PB.PatternBinding n, Just (PB.body -> PB.LiteralExpr (PB.body -> PB.FunctionLiteral {}))) -> do
          modify $ funcName ?~ (let PB.Name x = PB.body n in x)
        _ -> return ()

      case (maybeForAll, maybeType, maybeExpr) of
        (Nothing, _, _) -> do
          givenT <- maybe newTypeVar convertAstTypeToType maybeType

          -- Evaluate let body
          (unifyT, unifyS, ins) <- case maybeExpr of
            Just expr -> do
              (exprT, exprS, exprI) <- getNodeType expr
              (ty, sub) <- unifyTypes expr (applySub exprS givenT) exprT
              return (ty, sub, unwrapIns exprI)
            Nothing -> return (givenT, noSub, RB.ILiteral RB.UnInitV)

          (patS, _, exprP) <- ensurePatternMatches unifyT pat

          -- Reset fn name (for stack trace)
          case (PB.body pat, maybeExpr) of
            ( PB.PatternBinding _,
              Just (PB.body -> PB.LiteralExpr (PB.body -> PB.FunctionLiteral {}))
              ) -> do
                modify $ funcName .~ prevFnName
            _ -> return ()

          return (primitiveTy VoidT [], mergeSubs [unifyS, patS], Just $ RB.IPatBind exprP ins)
        (Just lForAll, _, _) -> case PB.body pat of
          PB.PatternBinding n -> do
            lookupName n >>= \case
              TraitSymbol tId ->
                -- Trait implementation:
                lookupTraitDef tId >>= \tDef -> do
                  modify $ over symbolScopes newSymbolTable
                  implBound <- convertForAllWithBinds lForAll

                  -- Adds gen type vars
                  (_, sub1, ctx) <- matchesBound n (view implBoundArgs implBound) (view traitBound tDef)
                  let traitDefT = applySub sub1 $ replaceTypeVars ctx (FunctionT $ view traitType tDef)

                  givenT <- applySub sub1 <$> maybe newTypeVar convertAstTypeToType maybeType
                  (unifyT1, unifyS1) <- unifyTypes n traitDefT givenT

                  -- Here we don't want to emit, because we do that when a trait is called.
                  (unifyT2, unifyS2) <- case maybeExpr of
                    Just expr -> do
                      (exprT, exprS, _) <- getNodeType expr
                      unifyTypes expr (applySub exprS unifyT1) (applySub unifyS1 exprT)
                    Nothing -> return (unifyT1, unifyS1)

                  -- Make sure the given type is a function.
                  let fnType = case unifyT2 of
                        FunctionT f -> f
                        _ -> internalPanicPure "Expected function type"
                  modifyTraitDef tId $ over traitImpls ((implBound, fnType, maybeExpr) :)
                  modify $ over symbolScopes popSymbolTable

                  -- Reset fn name
                  case (PB.body pat, maybeExpr) of
                    ( PB.PatternBinding _,
                      Just (PB.body -> PB.LiteralExpr (PB.body -> PB.FunctionLiteral {}))
                      ) -> do
                        modify $ funcName .~ prevFnName
                    _ -> return ()

                  return (primitiveTy VoidT [], unifyS2, Nothing)
              -- TODO: emit
              _ -> typeError n $ "Symbol '" ++ show n ++ "' is not a trait."
          _ -> typeError pat "Cannot use pattern in trait implementation."
    PB.Assign lhs rhs -> case PB.body lhs of
      (PB.Variable an) -> do
        -- Variable assignment
        (rhsT, rhsS, rhsI) <- getNodeType rhs
        (lhsT, lhsS, _) <- getNodeType lhs
        (t, sub) <- unifyTypes rhs (applySub rhsS lhsT) (applySub lhsS rhsT)
        varId <-
          lookupSymbol an >>= \case
            ValueSymbol varId _ -> return varId
            _ -> internalPanicPure "expected value symbol"
        return (t, sub, Just $ RB.INameSet (RB.Name varId) (unwrapIns rhsI))
      (PB.PropertyAccess expr name) -> do
        -- Struct assignment
        (propType, propS, propI) <- getNodeType expr
        case propType of
          (DefinedType tId tArgs) ->
            lookupTypeDef tId >>= \case
              StructT structTyp -> do
                case PB.body name `HM.lookup` view structFields structTyp of
                  Just f -> do
                    (rhsType, rhsS, rhsI) <- getNodeType rhs
                    let expectedFieldTy =
                          replaceTypeVars
                            (HM.fromList (zip (view (structBound . boundVars) structTyp) tArgs))
                            (view structFieldType f)
                    _ <- unifyTypes rhs expectedFieldTy (applySub propS rhsType)
                    -- Emit a PropSet instruction.
                    return
                      ( primitiveTy VoidT [],
                        mergeSubs [rhsS, propS],
                        Just $ RB.IPropSet (unwrapIns propI) (view structFieldIndex f) (unwrapIns rhsI)
                      )
                  Nothing -> typeError name $ "Struct type '" ++ show (view structName structTyp) ++ "' does not contain field '" ++ show name ++ "'."
              _ -> typeError expr "Cannot modify property of non-struct type."
          _ -> typeError expr "Cannot modify property of non-struct type."
      _ -> internalPanicPure "unexpected expression type in assignment"

-- | TypedNode helper for struct literals
getStructLiteralNodeType ::
  PB.AstNode PB.AccessName ->
  TypeId ->
  Maybe [PB.AstNode PB.Type] ->
  HM.HashMap (PB.AstNode PB.Name) (PB.AstNode PB.Expression) ->
  Typecheck (Type, Substitution, RB.Instruction)
getStructLiteralNodeType an tId maybeTypeArgs entries = do
  sTyp <- lookupTypeDef tId
  case sTyp of
    (StructT structTyp) -> do
      -- Parse the given type args
      givenTypeArgs <- case maybeTypeArgs of
        Nothing -> mapM (const newTypeVar) (view (structBound . boundVars) structTyp)
        Just ts -> mapM convertAstTypeToType ts
      (matchedTypeArgs, sub1, ctx) <- matchesBound an givenTypeArgs (view structBound structTyp)

      -- Get all the struct entries that were given.
      let entriesWithoutAstNode = HM.fromList $ map (first PB.body) (HM.toList entries) -- @Speed: this is slow.
      mapM_
        ( \(fieldName, field) ->
            case fieldName `HM.lookup` entriesWithoutAstNode of
              Just _ -> return ()
              Nothing -> case view structFieldDefault field of
                Just _ -> return ()
                Nothing -> typeError an $ "Missing required struct field '" ++ show fieldName ++ "'."
        )
        (HM.toList $ view structFields structTyp)

      -- Make sure the given entries are valid type-wise.
      subs <-
        mapM
          ( \(fieldName, fieldExpr) -> do
              case PB.body fieldName `HM.lookup` view structFields structTyp of
                Just fieldEntry -> do
                  (exprT, exprS, exprI) <- getNodeType fieldExpr
                  let expectedT = applySub (mergeSubs [sub1, exprS]) $ replaceTypeVars ctx (view structFieldType fieldEntry)
                  (_, sub3) <- unifyTypes fieldName expectedT exprT
                  return (mergeSubs [sub1, exprS, sub3], unwrapIns exprI) -- @Improvement: Is this really needed?
                Nothing ->
                  typeError fieldName $
                    "Struct type '" ++ show (view structName structTyp)
                      ++ "' does not have field '"
                      ++ show fieldName
                      ++ "'."
          )
          (HM.toList entries)

      -- Emit a struct literal instruction.
      let ins = RB.IStruct (V.fromList (map snd subs))
      let allSubs = mergeSubs (map fst subs)
      return (DefinedType tId (map (applySub allSubs) matchedTypeArgs), allSubs, ins)
    _ -> internalPanicPure "Expected struct type symbol"

-- | TypedNode helper for entire modules.
instance TypedNode PB.Module where
  getNodeType (PB.body -> (PB.Module k)) = do
    -- get the current module from state
    modsName <- gets $ view currentNamespace
    namespacesR <- gets $ view loadedNamespaces

    -- firstly check if the module has been previously loaded in, and if it has we just return
    -- the loaded SymbolScopeGroup
    case HM.lookup modsName namespacesR of
      Nothing -> do
        -- save the current scope,
        oldScope <- gets $ view symbolScopes
        initialSyms <- gets $ view initialSymbols
        modify $ symbolScopes .~ initialSyms

        -- run through all of the module statements and apply typechecking to all of the
        -- statements in the module, after returning
        (_, _, ins) <- unzip3 <$> mapM getNodeType k

        -- add the namespace as a new type and then return the type, and then restore the scope
        currSyms <- gets $ view (symbolScopes . currSymbolTable)
        k <- addType (NamespaceT modsName currSyms)
        modify $ symbolScopes .~ oldScope

        return (DefinedType k [], noSub, Just $ RB.ISeq (catMaybes ins)) -- TODO: is this right (emit)?
      Just l -> do
        let currSyms = view currSymbolTable l

        -- add namespace type to TypeTable and return it
        k <- addType (NamespaceT modsName currSyms)
        return (DefinedType k [], noSub, Nothing)

-- | TypedNode for Literal.
instance TypedNode PB.Literal where
  getNodeType n = case PB.body n of
    PB.StrLiteral s -> return (primitiveTy StrT [], noSub, Just $ RB.IStr (pack s))
    PB.CharLiteral c -> return (primitiveTy CharT [], noSub, Just $ RB.ILiteral (RB.CharV c))
    PB.IntLiteral i -> return (primitiveTy IntT [], noSub, Just $ RB.ILiteral (RB.IntV i))
    PB.FloatLiteral f -> return (primitiveTy FloatT [], noSub, Just $ RB.ILiteral (RB.FloatV f))
    PB.SetLiteral entries -> do
      (t, sub, entriesI) <- ensureSameType (PB.Name "T") (typeDefBound $ primitiveT SetT) entries
      hashFuncCall <- getHashFnNodeType n t sub
      return (primitiveTy SetT [t], sub, Just $ RB.ISet (unwrapIns hashFuncCall) entriesI)
    PB.ListLiteral entries -> do
      (t, sub, ins) <- ensureSameType (PB.Name "T") (typeDefBound $ primitiveT ListT) entries
      return (primitiveTy ListT [t], sub, Just $ RB.IList (V.fromList ins))
    PB.MapLiteral entries -> do
      ((k, v), sub, entriesI) <- ensureMapType (map ((\(PB.MapEntry k v) -> (k, v)) . PB.body) entries)
      hashFuncCall <- getHashFnNodeType n k sub
      return (primitiveTy MapT [k, v], sub, Just $ RB.IMap (unwrapIns hashFuncCall) entriesI)
    PB.TupleLiteral entries -> do
      (entries, sub, ins) <- getNodeTypes entries
      return (TupleT entries, sub, Just $ RB.ITuple (V.fromList ins))
    PB.StructLiteral ty entries -> do
      (an, tArgs) <- case PB.body ty of
        PB.NamedType accName typeArgs -> return (accName, typeArgs)
        _ -> internalPanicPure "expected name type in struct literal"
      (ty, sub, ins) <-
        lookupSymbol an >>= \case
          TypeConsSymbol tId -> case tArgs of
            [] -> getStructLiteralNodeType an tId Nothing entries
            _ -> getStructLiteralNodeType an tId (Just tArgs) entries
          TypeSymbol (DefinedType tId []) -> getStructLiteralNodeType an tId (Just tArgs) entries
          _ -> typeError an $ "Symbol '" ++ show an ++ "' is not a struct type."
      return (ty, sub, Just ins)
    PB.FunctionLiteral params maybeRet body -> do
      modify $ over symbolScopes newSymbolTable
      paramTs <-
        mapM
          ( \(PB.body -> PB.FunctionParam name maybeTy) -> do
              case maybeTy of
                Just ty -> (PB.body name,) <$> convertAstTypeToType ty
                Nothing -> (PB.body name,) <$> newTypeVar
          )
          params
      retT <- maybe newTypeVar convertAstTypeToType maybeRet

      (bodyT, bodyS, bodyI, paramSyms) <- enterFunction retT $ do
        -- Add the parameters as symbols
        paramSyms <-
          mapM
            ( \(n, t) -> do
                ValueSymbol idx _ <- addValueName n t
                return $ RB.Name idx
            )
            paramTs -- @Improvement: is this also necessary?
        (bodyT, bodyS, bodyI) <- getNodeType body

        gets (view retOnce) >>= \case
          True -> do
            bodyTMapped <- case bodyT of
              t | t == primitiveTy VoidT [] -> do
                gets $ fromMaybe (internalPanicPure "expected to be in fn") . view funcRetType
              t -> return t
            (realRetT, sub) <- ensureInFunction body (applySub bodyS bodyTMapped) "Unexpected error"
            return (realRetT, mergeSubs [sub, bodyS], bodyI, paramSyms)
          False -> do
            let retSignal = RB.IReturn (unwrapIns bodyI)
            return (bodyT, bodyS, Just retSignal, paramSyms)

      let unifiedParamTs = map (\(_, b) -> applySub bodyS b) paramTs
      (unifyT, unifyS) <- unifyTypes body (applySub bodyS retT) bodyT
      let unifyS2 = mergeSubs [unifyS, bodyS]
      let unifiedParamTs2 = map (applySub unifyS2) unifiedParamTs

      let functionTy = FunctionType unifiedParamTs2 unifyT paramSyms
      modify $ over symbolScopes popSymbolTable

      currFnName <- gets (fromMaybe "<unnamed>" . view funcName)
      let fnDef = RB.IFnDef currFnName paramSyms (unwrapIns bodyI)
      return (FunctionT functionTy, unifyS2, Just fnDef)

-- | TypedNode for Literal.
getFuncCallNodeType :: PB.AstNode PB.Expression -> PB.AstNode PB.FunctionCallArgs -> Typecheck (Type, Substitution, Maybe RB.Instruction)
getFuncCallNodeType subject astArgs@(PB.body -> (PB.FunctionCallArgs Nothing args src)) = do
  (subjectT, _, subjectI) <- getNodeType subject -- TODO: use substitution?
  case subjectT of
    FunctionT expected -> do
      (argsTs, argsS, argsI) <- getNodeTypes args
      (_, argsUS) <- unifyTypeList "Mismatching function arguments." astArgs (view funcArgs expected) argsTs
      let allS = mergeSubs [argsS, argsUS]
      let retT = applySub allS (view funcReturn expected)
      return (retT, allS, Just $ RB.IFnCall src (unwrapIns subjectI) argsI)
    _ -> do
      showSubjectT <- showType subjectT
      typeError subject $ "Type '" ++ showSubjectT ++ "' is not a function type."
getFuncCallNodeType _ a = typeError a "Cannot use generic arguments here."

getEnumLiteralNodeType ::
  PB.AstNode a ->
  TypeId ->
  PB.Name ->
  Maybe [PB.AstNode PB.Type] ->
  [PB.AstNode PB.Expression] ->
  Typecheck (Type, Substitution, Maybe RB.Instruction)
getEnumLiteralNodeType an tId name maybeTypeArgs args = do
  -- get the typedef and then find the enum, return the type of the variant
  eTyp <- lookupTypeDef tId
  case eTyp of
    (EnumT enumTyp) -> do
      modify $ over symbolScopes newSymbolTable
      let (variantArgs, variant) =
            fromMaybe
              (internalPanicPure "expected to find name in enum")
              (name `HM.lookup` view enumVariants enumTyp)

      givenTypeArgs <- case maybeTypeArgs of
        Nothing -> mapM (const newTypeVar) (view (enumBound . boundVars) enumTyp)
        Just ts -> mapM convertAstTypeToType ts
      (matchedTypeArgs, sub1, ctx) <- matchesBound an givenTypeArgs (view enumBound enumTyp)

      (givenArgs, sub2, argsI) <- getNodeTypes args
      let expectedArgs = map (replaceTypeVars ctx) variantArgs
      (_, sub3) <- unifyTypeList "Mismatching enum variant arguments." an expectedArgs givenArgs

      modify $ over symbolScopes popSymbolTable
      let allSubs = mergeSubs [sub1, sub2, sub3]

      return
        ( DefinedType tId (map (applySub allSubs) matchedTypeArgs),
          allSubs,
          Just $ RB.IEnum variant (V.fromList argsI)
        )
    _ -> internalPanicPure "Expected enum type symbol"

-- | Get the "hash" trait function definition for a given type.
--
-- This is used for maps and sets, and follows the structure of `getTraitCallNodeType`.
getHashFnNodeType ::
  PB.AstNode a ->
  Type ->
  Substitution ->
  Typecheck (Maybe RB.Instruction)
getHashFnNodeType n ty tSub = do
  modify $ over symbolScopes newSymbolTable
  traitDef <- lookupTraitDef (traitPrimitiveId HashTrait)

  fns <- traitImplsExist n (TraitBound (traitPrimitiveId HashTrait) [ty])
  found <-
    foldM
      ( \acc (fn, sub, fnAst) -> case acc of
          Just _ -> return acc
          Nothing -> do
            let (FunctionType tArgs _ _) = fn
            res <- lift . runExceptT $ unifyTypeList "Mismatching function trait arguments." n tArgs (map (applySub sub) [ty])
            case res of
              Left _ -> return Nothing
              Right (_, unifyS) -> return . Just $ (mergeSubs [unifyS, sub, tSub], fn, fnAst)
      )
      Nothing
      fns

  (unifyS, fn, fnAst) <- case found of
    Nothing ->
      typeError n $ "No matching implementation of trait '" ++ show (view traitName traitDef) ++ "' found."
    Just r -> return r

  typeVarBounds <- view boundTraitsData <$> getTypeVarBounds
  modify $ over symbolScopes popSymbolTable

  if hasTypeVars ty || not (null typeVarBounds)
    then return Nothing
    else do
      case fnAst of
        Just (f, ctx) -> do
          if hasGenTypeVars (applySub unifyS $ FunctionT fn)
            then do
              typeError n "Ambiguous type variables found. Consider adding more type annotations."
            else do
              oldCtx <- gets . view $ typeVarCtx
              modify $ typeVarCtx .~ ctx
              (_, _, bodyI) <- getNodeType f
              modify $ typeVarCtx .~ oldCtx
              return bodyI
        Nothing -> do
          if hasGenTypeVars (applySub unifyS $ FunctionT fn)
            then return Nothing
            else typeError n "Cannot find a concrete implementation for this trait call. Consider adding more type bounds."

-- | Evaluate a trait call, by first emitting the function definition with
-- replaced type variables, and then emitting the function call itself.
--
-- An @Improvement to this would be to cache emitted functions to prevent having
-- to inline them each time.
getTraitCallNodeType ::
  PB.AstNode PB.AccessName ->
  TraitId ->
  Maybe [PB.AstNode PB.Type] ->
  String ->
  [PB.AstNode PB.Expression] ->
  Typecheck (Type, Substitution, Maybe RB.Instruction)
getTraitCallNodeType an tId maybeTypeArgs src args = do
  modify $ over symbolScopes newSymbolTable
  -- Get the trait definition
  traitDef <- lookupTraitDef tId

  -- Resolve the given trait type arguments
  givenTypeArgs <- case maybeTypeArgs of
    Nothing -> mapM (const newTypeVar) (view (traitBound . boundVars) traitDef)
    Just ts -> mapM convertAstTypeToType ts

  -- Resolve the given trait function arguments
  (givenArgsT, givenArgsS, argsI) <- getNodeTypes args

  -- Get all the trait implementations that match the constraints we are given.
  fns <- traitImplsExist an (TraitBound tId givenTypeArgs)
  found <-
    foldM
      ( \acc (fn, sub, fnAst) -> case acc of
          Just _ -> return acc
          Nothing -> do
            let (FunctionType tArgs _ _) = fn
            res <- lift . runExceptT $ unifyTypeList "Mismatching function trait arguments." an (map (applySub givenArgsS) tArgs) (map (applySub sub) givenArgsT)
            case res of
              Left _ -> return Nothing
              Right (_, unifyS) -> return . Just $ (mergeSubs [unifyS, sub], fn, fnAst)
      )
      Nothing
      fns
  (unifyS, fn, fnAst) <- case found of
    Nothing ->
      typeError an $ "No matching implementation of trait '" ++ show (view traitName traitDef) ++ "' found."
    Just r -> return r

  let allSubs = mergeSubs [givenArgsS, unifyS]

  typeVarBounds <- view boundTraitsData <$> getTypeVarBounds
  modify $ over symbolScopes popSymbolTable

  -- Potentially emit trait call if no GenTypeVars exist in the resolved type.
  if any hasTypeVars givenTypeArgs || not (null typeVarBounds)
    then return (applySub allSubs (view funcReturn fn), allSubs, Nothing)
    else do
      case fnAst of
        Just (f, ctx) -> do
          if hasGenTypeVars (applySub allSubs $ FunctionT fn)
            then do
              typeError an "Ambiguous type variables found. Consider adding more type annotations."
            else do
              oldCtx <- gets . view $ typeVarCtx
              modify $ typeVarCtx .~ ctx
              (_, _, bodyI) <- getNodeType f
              modify $ typeVarCtx .~ oldCtx
              -- Emit trait call.
              let callIns = RB.IFnCall src (unwrapIns bodyI) argsI
              return (applySub allSubs (view funcReturn fn), allSubs, Just callIns)
        Nothing -> do
          if hasGenTypeVars (applySub allSubs $ FunctionT fn)
            then return (applySub allSubs (view funcReturn fn), allSubs, Nothing)
            else typeError an "Cannot find a concrete implementation for this trait call. Consider adding more type bounds."

-- Convert a tuple property access into an index.
tupleProp :: PB.AstNode PB.Name -> Typecheck Int
tupleProp name = case (let PB.Name n = PB.body name in n) of
  "first" -> return 0
  "second" -> return 1
  "third" -> return 2
  "fourth" -> return 3
  "fifth" -> return 4
  "sixth" -> return 5
  "seventh" -> return 6
  "eighth" -> return 7
  "ninth" -> return 8
  _ ->
    typeError name $
      "Invalid tuple property access '" ++ show name ++ "'. Use pattern matching to access tuples longer than 9."

-- TypedNode instance for Expression
instance TypedNode PB.Expression where
  getNodeType n = case PB.body n of
    PB.FunctionCall subject astArgs@(PB.body -> (PB.FunctionCallArgs maybeTypeArgs args src)) -> do
      case PB.body subject of
        PB.IntrinsicExpr (PB.IntrinsicKey key) -> do
          -- Intrinsic resolution
          case key `HM.lookup` intrinsicMap of
            Nothing -> typeError n $ "Intrinsic '" ++ key ++ "' not found."
            Just (intrinsicRef, _) -> do
              (_, argsS, argsI) <- getNodeTypes args
              retT <- newTypeVar
              let ins = RB.IFnCall ("#" ++ key) (RB.IRef intrinsicRef) argsI
              return (retT, argsS, Just ins)
        -- This can either be a function call, a trait call or an enum literal.
        PB.Variable an ->
          lookupSymbol an >>= \case
            EnumVariantSymbol tId name -> getEnumLiteralNodeType an tId name maybeTypeArgs args
            TraitSymbol tId -> getTraitCallNodeType an tId maybeTypeArgs src args
            _ -> getFuncCallNodeType subject astArgs
        _ -> getFuncCallNodeType subject astArgs
    PB.LogicalOp op a b -> do
      (aT, _, aI) <- getNodeType a
      _ <- unifyTypes a (enumPrimitiveTy BoolT []) aT
      (bT, _, bI) <- getNodeType b
      _ <- unifyTypes b (enumPrimitiveTy BoolT []) bT
      let ins = case op of
            PB.LogicalOrOp -> RB.ILogicalOr (unwrapIns aI) (unwrapIns bI)
            PB.LogicalAndOp -> RB.ILogicalAnd (unwrapIns aI) (unwrapIns bI)
      return (enumPrimitiveTy BoolT [], noSub, Just ins)
    PB.Variable an -> do
      sym <- lookupSymbol an
      case sym of
        ValueSymbol idx ty -> return (ty, noSub, Just $ RB.INameRef (RB.Name idx))
        EnumVariantSymbol tId name -> getEnumLiteralNodeType an tId name Nothing []
        _ -> typeError an $ "Symbol '" ++ show an ++ "' is not a variable."
    PB.LiteralExpr l -> getNodeType l
    PB.TypedExpression expr ty -> do
      tyT <- convertAstTypeToType ty
      (exprT, exprS, exprI) <- getNodeType expr
      (ty, sub) <- unifyTypes expr (applySub exprS tyT) exprT
      return (ty, sub, exprI)
    PB.BlockExpr b -> getNodeType b
    PB.PropertyAccess expr name -> do
      (propType, propS, propI) <- getNodeType expr
      case propType of
        -- Structs
        (DefinedType tId tArgs) ->
          lookupTypeDef tId >>= \case
            StructT structTyp -> do
              case PB.body name `HM.lookup` view structFields structTyp of
                Just f -> do
                  let expectedFieldTy =
                        replaceTypeVars
                          (HM.fromList (zip (view (structBound . boundVars) structTyp) tArgs))
                          (view structFieldType f)
                  return (expectedFieldTy, propS, Just $ RB.IPropGet (unwrapIns propI) (view structFieldIndex f))
                Nothing -> typeError name $ "Struct type '" ++ show (view structName structTyp) ++ "' does not contain field '" ++ show name ++ "'."
            _ -> typeError expr "Cannot access property of non-struct/non-tuple type."
        -- Tuples
        (TupleT tArgs) -> do
          idx <- tupleProp name
          when (idx >= length tArgs) $ do
            tShow <- showType propType
            typeError name $ "Tuple type '" ++ tShow ++ "' does not have a " ++ show name ++ " element."
          let tArg = tArgs !! idx
          return (tArg, propS, Just $ RB.IPropGet (unwrapIns propI) idx)
        _ -> typeError expr "Cannot access property of non-struct/non-tuple type."
    PB.Import (PB.body -> (PB.StrLiteral path)) -> do
      wd <- gets $ views currentNamespace takeDirectory
      realPath <- liftIO $ U.resolveModPath wd path

      -- Resolve module to typecheck
      mods <- gets $ view modules
      case realPath `HM.lookup` mods of
        Just resolvedMod -> do
          let moduleToParse = view PB.moduleNode resolvedMod

          oldName <- gets $ view currentNamespace
          modify $ currentNamespace .~ view PB.realModulePath resolvedMod

          -- Typecheck the module.
          t <- getNodeType moduleToParse

          modify $ currentNamespace .~ oldName
          return t
        _ -> internalPanicPure "could not find import"
    -- Since imports can only have string literals, if this for some reason reaches this case,
    -- we should panic since it is not expected!
    PB.Import _ -> internalPanicPure "Invalid import AST node, expected string literal"
    PB.IntrinsicExpr (PB.IntrinsicKey _) -> typeError n "Cannot resolve type of intrinsic, please call it as a function."
