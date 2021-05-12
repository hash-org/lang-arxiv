{-# LANGUAGE TupleSections #-}

-- | Hash Compiler type inference logic.
-- | All rights reserved 2021 (c) The Hash Language authors
--
-- This module implements the Hindley-Milner algorithm for type inference.
module Typecheck.Infer where

import Control.Error (catMaybes, mapMaybe, runExceptT)
import Control.Lens (over, view)
import Control.Monad (foldM, unless)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Foldable (foldrM)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Panic
import Parse.Boot (Name)
import qualified Parse.Boot as PB
import Typecheck.Boot
import Utils (zipMaybe)

-- | An empty substitution.
noSub :: Substitution
noSub = Substitution HM.empty

-- | Merge a list of substitutions into a single substitution.
mergeSubs :: [Substitution] -> Substitution
mergeSubs subs =
  Substitution $
    HM.map
      ( \v -> case v of
          GenTypeVar t -> HM.lookupDefault v t merged
          _ -> v
      )
      merged
  where
    (Substitution merged) =
      foldr
        (\s -> over subMap (HM.union (view subMap s) . HM.map (applySub s)))
        noSub
        subs

-- | Replace GenTypeVars with other types, in a given type.
applySub :: Substitution -> Type -> Type
applySub replaceMap ty = case ty of
  DefinedType tId types -> DefinedType tId (map (applySub replaceMap) types)
  TypeVar _ -> ty
  TupleT types -> TupleT $ map (applySub replaceMap) types
  FunctionT (FunctionType args ret names) ->
    FunctionT $
      FunctionType
        (map (applySub replaceMap) args)
        (applySub replaceMap ret)
        names
  GenTypeVar t -> let Substitution m = replaceMap in HM.lookupDefault ty t m
  ExistentialT -> ty

-- | Check whether a given type contains type variables.
hasTypeVars :: Type -> Bool
hasTypeVars ty = case ty of
  DefinedType _ types -> any hasTypeVars types
  TypeVar _ -> True
  TupleT types -> any hasTypeVars types
  FunctionT (FunctionType args ret _) -> any hasTypeVars args || hasTypeVars ret
  GenTypeVar _ -> False
  ExistentialT -> False

-- | Check whether a given type contains generated type variables.
hasGenTypeVars :: Type -> Bool
hasGenTypeVars ty = case ty of
  DefinedType _ types -> any hasGenTypeVars types
  TypeVar _ -> False
  TupleT types -> any hasGenTypeVars types
  FunctionT (FunctionType args ret _) -> any hasGenTypeVars args || hasGenTypeVars ret
  GenTypeVar _ -> True
  ExistentialT -> False

-- | Replace TypeVars with other types, in a given type.
replaceTypeVars :: HM.HashMap Name Type -> Type -> Type
replaceTypeVars ctx ty = case ty of
  DefinedType tId types -> DefinedType tId (map (replaceTypeVars ctx) types)
  TypeVar v -> HM.lookupDefault ty v ctx
  TupleT types -> TupleT (map (replaceTypeVars ctx) types)
  FunctionT (FunctionType args ret names) ->
    FunctionT
      ( FunctionType
          (map (replaceTypeVars ctx) args)
          (replaceTypeVars ctx ret)
          names
      )
  GenTypeVar _ -> ty
  ExistentialT -> ty

-- | Replace TypeVars with other types, in a given list of trait bounds.
replaceTypeVarsInBoundTraits :: HM.HashMap Name Type -> BoundTraits -> BoundTraits
replaceTypeVarsInBoundTraits ctx (BoundTraits bt) =
  BoundTraits $ map (first . over traitBoundArgs $ map (replaceTypeVars ctx)) bt

-- | Create a mapping from a set of type variable names to generated types.
createGenTypeVarCtx :: [Name] -> Typecheck (HM.HashMap Name Type)
createGenTypeVarCtx names = HM.fromList <$> mapM (\n -> (n,) <$> newTypeVar) names

-- | Check whether a given type argument list matches a trait implementation bound.
matchesImplBound :: PB.AstNode a -> [Type] -> ImplBound -> Typecheck ([Type], Substitution, HM.HashMap Name Type)
matchesImplBound n tArgs (ImplBound iNames iArgs iTraits) = do
  genTypeVarCtx <- createGenTypeVarCtx (HS.toList iNames)

  -- Create the version of the type arguments that replaces type
  -- variables with the generated type variables above.
  let instantiatedArgs = map (replaceTypeVars genTypeVarCtx) iArgs

  -- Do the same with trait bounds.
  let instantiatedArgsBoundTraits = replaceTypeVarsInBoundTraits genTypeVarCtx iTraits

  -- Set the trait bounds of the type to the state.
  -- @Improvement: We set the same bound for each of the generated types,
  -- this could be more efficient.
  mapM_
    ( \case
        GenTypeVar g -> addGenTypeVarBounds g instantiatedArgsBoundTraits
        _ -> internalPanicPure "expected generated type variables in genTypeVarCtx"
    )
    (HM.elems genTypeVarCtx)
  (ts, sub) <- unifyTypeList "Mismatching bound parameters." n instantiatedArgs tArgs
  return (ts, sub, genTypeVarCtx)

-- | Checks that first bound is "subbound" of second.
--
-- Returns additional bounds picked up by second.
matchesBound :: PB.AstNode a -> [Type] -> Bound -> Typecheck ([Type], Substitution, HM.HashMap Name Type)
matchesBound n tArgs (Bound bVars bTraits) =
  matchesImplBound n tArgs (ImplBound (HS.fromList bVars) (TypeVar <$> bVars) bTraits)

-- | Check whether a given trait implementation actually exists, or there
-- exists one which matches it.
traitImplsExist ::
  PB.AstNode a ->
  TraitBound ->
  Typecheck
    [ ( FunctionType,
        Substitution,
        Maybe (PB.AstNode PB.Expression, HM.HashMap Name Type)
      )
    ]
traitImplsExist n (TraitBound tId tArgs) = do
  let genTypeVars =
        mapMaybe
          ( \case
              GenTypeVar t -> Just t
              _ -> Nothing
          )
          tArgs

  -- Left: trait bounds, assume they exist
  -- Light: actual traitImpls
  fromGenTypeVars <-
    concat
      <$> mapM (fmap (view boundTraitsData) . getGenTypeVarBounds) genTypeVars
  fromTypeVars <- view boundTraitsData <$> getTypeVarBounds
  fromImpls <- view traitImpls <$> lookupTraitDef tId
  let allImpls = concat [Right <$> fromImpls, Left <$> fromTypeVars, Left <$> fromGenTypeVars]

  -- Try to resolve implementations:
  res <-
    mapM
      ( \case
          Left (TraitBound bTraitId bTraitArgs, fn) -> do
            if bTraitId == tId
              then do
                found <- lift . runExceptT $ unifyTypeList "Trait type arguments mismatch in length" n bTraitArgs tArgs
                case found of
                  Left _ -> return Nothing
                  Right (_, sub) -> return (Just (fn, sub, Nothing))
              else return Nothing
          Right (iBound, fn, fnAst) -> do
            found <- lift . runExceptT $ matchesImplBound n tArgs iBound
            case found of
              Left _ -> return Nothing
              Right (_, sub, ctx) -> do
                let FunctionT fnRep = replaceTypeVars ctx (FunctionT fn)
                return (Just (fnRep, sub, (,ctx) <$> fnAst))
      )
      allImpls
  return $ catMaybes res

-- | Unify two lists of types, returning the unified list and any substitutions.
unifyTypeList :: String -> PB.AstNode a -> [Type] -> [Type] -> Typecheck ([Type], Substitution)
unifyTypeList msg n as bs = do
  foldrM
    ( \(ma, mb) (ts, sub) -> do
        case (ma, mb) of
          (Just a, Just b) -> do
            (t, s) <- unifyTypes n a b
            return (t : ts, mergeSubs [s, sub])
          _ -> typeError n $ msg ++ " Expected " ++ show (length as) ++ " but got " ++ show (length bs) ++ "."
    )
    ([], noSub)
    (zipMaybe as bs)

-- | The difference with this implementation is that it takes a list of ASTNode types
-- | rather than just types
unifyTypeListWithAst :: String -> PB.AstNode a -> [(PB.AstNode b, Type)] -> [Type] -> Typecheck ([Type], Substitution)
unifyTypeListWithAst msg n as bs =
  foldM
    ( \(ts, sub) (ma, mb) -> do
        case (ma, mb) of
          (Just (_, a), Just b) -> do
            (t, s) <- unifyTypes n a b
            return (t : ts, mergeSubs [s, sub])
          _ -> typeError n $ msg ++ " Expected " ++ show (length as) ++ " but got " ++ show (length bs) ++ "."
    )
    ([], noSub)
    (zipMaybe as bs)

-- | Throw a type mismatch error.
typeMismatch :: PB.AstNode a -> String -> String -> Typecheck b
typeMismatch n sa sb = typeError n $ "Cannot match type '" ++ sa ++ "' with '" ++ sb ++ "'."

-- | Unify two primitive types.
unifyPrimitives ::
  PB.AstNode a ->
  (Primitive, [Type]) ->
  (Primitive, [Type]) ->
  Typecheck ((Primitive, [Type]), Substitution)
unifyPrimitives n a b = case (a, b) of
  ((StrT, []), (StrT, [])) -> return ((StrT, []), noSub)
  ((CharT, []), (CharT, [])) -> return ((CharT, []), noSub)
  ((FloatT, []), (FloatT, [])) -> return ((FloatT, []), noSub)
  ((IntT, []), (IntT, [])) -> return ((IntT, []), noSub)
  ((VoidT, []), (VoidT, [])) -> return ((VoidT, []), noSub)
  ((ListT, [ta]), (ListT, [tb])) -> do
    (tu, sub) <- unifyTypes n ta tb
    return ((ListT, [tu]), sub)
  ((SetT, [ta]), (SetT, [tb])) -> do
    (tu, sub) <- unifyTypes n ta tb
    return ((SetT, [tu]), sub)
  ((MapT, [ka, va]), (MapT, [kb, vb])) -> do
    (k, kSub) <- unifyTypes n ka kb
    (v, vSub) <- unifyTypes n va vb
    return ((MapT, [k, v]), mergeSubs [kSub, vSub])
  _ -> internalPanicPure "invalid primitive"

-- | Unify two `TypeDef` types.
--
-- Returns their unified type arguments.
--
-- The assumption is made that their type IDs are the same.
unifyTypeDefs :: PB.AstNode a -> (TypeDef, [Type]) -> (TypeDef, [Type]) -> Typecheck ([Type], Substitution)
unifyTypeDefs n a b = case (a, b) of
  ((StructT _, argsA), (StructT _, argsB)) -> unifyTypeList "Mismatching struct type parameters." n argsA argsB
  ((EnumT _, argsA), (EnumT _, argsB)) -> unifyTypeList "Mismatching enum type parameters." n argsA argsB
  ((NamespaceT moduleA _, []), (NamespaceT moduleB _, [])) | moduleA == moduleB -> return ([], noSub)
  ((NamespaceT {}, _), (NamespaceT {}, _)) -> internalPanicPure "Unexpected namespace type arguments."
  ((PrimitiveT _, _), (PrimitiveT _, _)) -> internalPanicPure "primitives shouldn't have reached here"
  ((tA, argsA), (tB, argsB)) -> do
    sa <- showTypeDef argsA tA
    sb <- showTypeDef argsB tB
    typeMismatch n sa sb

-- | Unify two types.
--
-- Returns the unified type as well as any substitutions.
--
-- The assumption is made that their type IDs are the same.
unifyTypes :: PB.AstNode a -> Type -> Type -> Typecheck (Type, Substitution)
unifyTypes n a b = case (a, b) of
  (ExistentialT, _) -> return (b, noSub)
  (_, ExistentialT) -> return (a, noSub)
  (GenTypeVar tIdA, GenTypeVar tIdB) -> do
    -- Set both to have same bounds
    boundsA <- getGenTypeVarBounds tIdA
    boundsB <- getGenTypeVarBounds tIdB
    addGenTypeVarBounds tIdA boundsB
    addGenTypeVarBounds tIdB boundsA
    return (a, Substitution (HM.singleton tIdA b))
  (GenTypeVar tIdA, _) -> do
    -- Ensure the other type implements all traits of the type variable:
    (BoundTraits varTraits) <- getGenTypeVarBounds tIdA
    let replacedVarTraits =
          map
            (first . over traitBoundArgs $ map (applySub (Substitution $ HM.singleton tIdA b)))
            varTraits
    subs <- mapM (traitImplsExist n . fst) replacedVarTraits
    let ok = (not . any null) subs
    unless ok $ typeError n "Type does not implement some required traits." -- @Improvement: descriptive

    -- If all went well, substitute and return subs.
    return (b, mergeSubs (Substitution (HM.singleton tIdA b) : map (\(_, b, _) -> b) (concat subs)))
  (_, GenTypeVar tIdB) -> return (a, Substitution (HM.singleton tIdB a))
  (TypeVar ta, TypeVar tb) | ta == tb -> return (a, noSub)
  (FunctionT ta, FunctionT tb) -> do
    let aArgs = view funcArgs ta
    let bArgs = view funcArgs tb
    (args, argsSub) <- unifyTypeList "Function arguments mismatch in length." n aArgs bArgs
    let aRet = applySub argsSub $ view funcReturn ta
    let bRet = applySub argsSub $ view funcReturn tb
    (ret, retSub) <- unifyTypes n aRet bRet
    return
      ( FunctionT $ FunctionType args ret (view runtimeFuncArgNames ta),
        mergeSubs [argsSub, retSub]
      )
  (TupleT tas, TupleT tbs) -> do
    (ts, sub) <- unifyTypeList "Tuples mismatch in length." n tas tbs
    return (TupleT ts, sub)
  -- Special handling for never.
  (_, DefinedType tId []) | tId == primitiveId NeverT -> return (DefinedType (primitiveId NeverT) [], noSub)
  (DefinedType tId [], _) | tId == primitiveId NeverT -> typeMismatch n "never" =<< showType b
  (DefinedType tIdA typeArgsA, DefinedType tIdB typeArgsB) | tIdA == tIdB -> do
    t <- lookupTypeDef tIdA -- a and b are the same
    case t of
      PrimitiveT (PrimitiveType tP _) -> do
        ((prim, typeArgs), sub) <- unifyPrimitives n (tP, typeArgsA) (tP, typeArgsB)
        return (DefinedType (primitiveId prim) typeArgs, sub)
      _ -> do
        (unifiedArgs, sub) <- unifyTypeDefs n (t, typeArgsA) (t, typeArgsB)
        return (DefinedType tIdA unifiedArgs, sub)
  _ -> do
    sa <- showType a
    sb <- showType b
    typeMismatch n sa sb
