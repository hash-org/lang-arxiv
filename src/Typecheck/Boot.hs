{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Hash Compiler types and primitives for type checking.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Typecheck.Boot where

import Control.Error (ExceptT, fromMaybe, runExceptT, throwE)
import Control.Lens (makeLenses, over, view, views, (+~))
import Control.Monad.State (StateT (runStateT), gets, modify)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (intercalate, isPrefixOf)
import Error (HashError (..))
import GHC.Generics (Generic)
import Panic (internalPanicPure)
import qualified Parse.Ast as A
import Parse.Boot (Name (Name))
import qualified Parse.Boot as PB
import qualified Runtime.Boot as RB
import Runtime.Intrinsics (intrinsicMap)

-- | Represents a symbol in the program.
data Symbol
  = TypeConsSymbol TypeId
  | ValueSymbol Int Type
  | TypeSymbol Type
  | EnumVariantSymbol TypeId Name
  | TraitSymbol TraitId
  deriving (Eq, Show)

-- | Represents a defined trait.
data Trait = Trait
  { _traitName :: Name,
    _traitBound :: Bound,
    _traitType :: FunctionType,
    _traitImpls :: [(ImplBound, FunctionType, Maybe (PB.AstNode PB.Expression))]
  }
  deriving (Eq, Show)

-- | Represents a function signature.
data FunctionType = FunctionType
  { _funcArgs :: [Type],
    _funcReturn :: Type,
    _runtimeFuncArgNames :: [RB.Name]
  }
  deriving (Eq, Show)

-- | A set of bounded traits, along with their resolved function types.
newtype BoundTraits = BoundTraits {_boundTraitsData :: [(TraitBound, FunctionType)]}
  deriving (Eq, Show)

-- | A for-all bound, comprised of bounded variables and traits.
data Bound = Bound
  { _boundVars :: [Name],
    _boundTraits :: BoundTraits
  }
  deriving (Eq, Show)

-- | A trait implementation bound, same as `Bound` but contains arbitrary types
-- for arguments.
data ImplBound = ImplBound
  { _implBoundNames :: HS.HashSet Name,
    _implBoundArgs :: [Type],
    _implBoundTraits :: BoundTraits
  }
  deriving (Eq, Show)

-- | A trait with a given list of types as instantiation constraints.
data TraitBound = TraitBound {_traitBoundId :: TraitId, _traitBoundArgs :: [Type]}
  deriving (Eq, Show)

-- | Represents a struct field.
data StructField = StructField
  { _structFieldType :: Type,
    _structFieldIndex :: Int,
    _structFieldDefault :: Maybe RB.Instruction
  }
  deriving (Eq, Show)

-- | Represents a struct definition.
data StructType = StructType
  { _structName :: PB.Name,
    _structBound :: Bound,
    _structFields :: HM.HashMap PB.Name StructField
  }
  deriving (Eq, Show)

-- | Represents an enum definition.
data EnumType = EnumType
  { _enumName :: PB.Name,
    _enumBound :: Bound,
    _enumVariants :: HM.HashMap PB.Name ([Type], RB.EnumVariant) -- second value is runtime index.
  }
  deriving (Eq, Show)

-- | Represents a primitive type in the program.
data Primitive
  = NeverT
  | VoidT
  | ListT
  | MapT
  | SetT
  | StrT
  | CharT
  | FloatT
  | IntT
  | NativeT
  deriving (Eq, Show, Generic, Enum, Bounded)

instance Hashable Primitive

-- | Represents a primitive enum type in the program.
data EnumPrimitive = BoolT | ResultT | OptionT
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Hashable EnumPrimitive

-- | A primitive type with some optional type bounds.
data PrimitiveType = PrimitiveType
  { _prim :: Primitive,
    _primBound :: Bound
  }
  deriving (Eq, Show)

-- | Represents a type.
data Type
  = DefinedType TypeId [Type]
  | TypeVar Name
  | GenTypeVar Int
  | ExistentialT
  | FunctionT FunctionType
  | TupleT [Type]
  deriving (Eq, Show)

-- | Represents a user-defined type.
data TypeDef
  = StructT StructType
  | EnumT EnumType
  | PrimitiveT PrimitiveType
  | NamespaceT String SymbolTable
  deriving (Eq, Show)

-- | Get the bound of a given typedef.
typeDefBound :: TypeDef -> Bound
typeDefBound = \case
  StructT t -> _structBound t
  EnumT t -> _enumBound t
  PrimitiveT t -> _primBound t
  NamespaceT {} -> emptyBound

-- | Map index for user-defined types in type table.
newtype TypeId = TypeId Int
  deriving (Eq, Show, Generic)

instance Hashable TypeId

-- | Map index for user-defined traits in type table.
newtype TraitId = TraitId Int
  deriving (Eq, Show, Generic)

instance Hashable TraitId

-- | Table that holds all user-defined types.
data TypeTable = TypeTable
  { _tableData :: HM.HashMap TypeId TypeDef,
    _traitData :: HM.HashMap TraitId Trait
  }
  deriving (Show)

-- | Table that holds all user-defined and primitive symbols that are
-- available.
data SymbolTable = SymbolTable
  { _symbolData :: HM.HashMap Name Symbol,
    _typeVarBounds :: BoundTraits
  }
  deriving (Eq, Show)

-- | Represents a group of symbol tables (corresponding to scopes).
data SymbolScopeGroup = SymbolScopeGroup {_currSymbolTable :: SymbolTable, _restSymbolTables :: [SymbolTable]}
  deriving (Show)

emptyBoundTraits :: BoundTraits
emptyBoundTraits = BoundTraits []

emptyBound :: Bound
emptyBound = Bound [] emptyBoundTraits

-- | Represents a primitive trait.
data TraitPrimitive = HashTrait | EqTrait
  deriving (Eq, Show, Generic, Bounded, Enum)

instance Hashable TraitPrimitive

-- | The list of initial primitive traits.
traitPrimitives :: HM.HashMap TraitPrimitive Trait
traitPrimitives =
  HM.fromList
    [ ( HashTrait,
        Trait
          (Name "hash")
          ( Bound
              [Name "T"]
              emptyBoundTraits
          )
          ( FunctionType
              [TypeVar (Name "T")]
              (primitiveTy IntT [])
              [freeRuntimeSymbol 1]
          )
          []
      ),
      ( EqTrait,
        Trait
          (Name "eq")
          ( Bound
              [Name "T"]
              emptyBoundTraits
          )
          ( FunctionType
              [TypeVar (Name "T"), TypeVar (Name "T")]
              (enumPrimitiveTy BoolT [])
              [freeRuntimeSymbol 2]
          )
          []
      )
    ]

traitPrimitiveId :: TraitPrimitive -> TraitId
traitPrimitiveId = TraitId . fromEnum

traitPrimitive :: TraitPrimitive -> Trait
traitPrimitive = fromMaybe (internalPanicPure "could not find trait primitive") . (`HM.lookup` traitPrimitives)

-- | The list of initial primitive enums.
enumPrimitives :: HM.HashMap EnumPrimitive TypeDef
enumPrimitives =
  HM.fromList
    [ ( BoolT,
        EnumT
          ( EnumType
              (Name "bool")
              emptyBound
              ( HM.fromList
                  [ (Name "true", ([], freeRuntimeEnumVariant 1)),
                    (Name "false", ([], freeRuntimeEnumVariant 0))
                  ]
              )
          )
      ),
      ( ResultT,
        EnumT
          ( EnumType
              (Name "Result")
              (Bound [Name "T", Name "E"] emptyBoundTraits)
              ( HM.fromList
                  [ (Name "Ok", ([TypeVar (Name "T")], freeRuntimeEnumVariant 3)),
                    (Name "Err", ([TypeVar (Name "E")], freeRuntimeEnumVariant 4))
                  ]
              )
          )
      ),
      ( OptionT,
        EnumT
          ( EnumType
              (Name "Option")
              (Bound [Name "T"] emptyBoundTraits)
              ( HM.fromList
                  [ (Name "Some", ([TypeVar (Name "T")], freeRuntimeEnumVariant 5)),
                    (Name "None", ([], freeRuntimeEnumVariant 6))
                  ]
              )
          )
      )
    ]

enumPrimitiveTy :: EnumPrimitive -> [Type] -> Type
enumPrimitiveTy prim = DefinedType (enumPrimitiveId prim)

enumPrimitiveT :: EnumPrimitive -> TypeDef
enumPrimitiveT = fromMaybe (internalPanicPure "could not find enum primitive") . (`HM.lookup` enumPrimitives)

enumPrimitiveId :: EnumPrimitive -> TypeId
enumPrimitiveId prim = TypeId $ fromEnum prim + fromEnum (maxBound :: Primitive) + 1

-- | The list of initial primitive types.
primitives :: HM.HashMap Primitive TypeDef
primitives =
  HM.fromList
    [ (FloatT, PrimitiveT (PrimitiveType FloatT emptyBound)),
      (IntT, PrimitiveT (PrimitiveType IntT emptyBound)),
      (CharT, PrimitiveT (PrimitiveType CharT emptyBound)),
      (StrT, PrimitiveT (PrimitiveType StrT emptyBound)),
      (NeverT, PrimitiveT (PrimitiveType NeverT emptyBound)),
      (VoidT, PrimitiveT (PrimitiveType VoidT emptyBound)),
      (ListT, PrimitiveT (PrimitiveType ListT (Bound [Name "T"] emptyBoundTraits))),
      (NativeT, PrimitiveT (PrimitiveType NativeT emptyBound)),
      -- Set:
      ( SetT,
        PrimitiveT
          ( PrimitiveType
              SetT
              ( Bound
                  [Name "T"]
                  ( BoundTraits
                      [ ( TraitBound (traitPrimitiveId HashTrait) [TypeVar (Name "T")],
                          FunctionType [TypeVar (Name "T")] (primitiveTy IntT []) [freeRuntimeSymbol 5]
                        ),
                        ( TraitBound (traitPrimitiveId EqTrait) [TypeVar (Name "T")],
                          FunctionType
                            [TypeVar (Name "T"), TypeVar (Name "T")]
                            (enumPrimitiveTy BoolT [])
                            [freeRuntimeSymbol 6]
                        )
                      ]
                  )
              )
          )
      ),
      -- Map:
      ( MapT,
        PrimitiveT
          ( PrimitiveType
              MapT
              ( Bound
                  [Name "K", Name "V"]
                  ( BoundTraits
                      [ ( TraitBound (traitPrimitiveId HashTrait) [TypeVar (Name "K")],
                          FunctionType [TypeVar (Name "K")] (primitiveTy IntT []) [freeRuntimeSymbol 7]
                        ),
                        ( TraitBound (traitPrimitiveId EqTrait) [TypeVar (Name "K")],
                          FunctionType
                            [TypeVar (Name "K"), TypeVar (Name "K")]
                            (enumPrimitiveTy BoolT [])
                            [freeRuntimeSymbol 7]
                        )
                      ]
                  )
              )
          )
      )
    ]

primitiveTy :: Primitive -> [Type] -> Type
primitiveTy prim = DefinedType (primitiveId prim)

primitiveT :: Primitive -> TypeDef
primitiveT p = fromMaybe (internalPanicPure "could not find primitive") (p `HM.lookup` primitives)

primitiveId :: Primitive -> TypeId
primitiveId = TypeId . fromEnum

-- | The list of initial symbols (from primitive types and values).
initialSymbolTable :: HM.HashMap Name Symbol
initialSymbolTable =
  HM.fromList -- Add all the primitive symbols to the root scope.
    [ (Name "float", TypeSymbol (DefinedType (primitiveId FloatT) [])),
      (Name "char", TypeSymbol (DefinedType (primitiveId CharT) [])),
      (Name "int", TypeSymbol (DefinedType (primitiveId IntT) [])),
      (Name "str", TypeSymbol (DefinedType (primitiveId StrT) [])),
      (Name "never", TypeSymbol (DefinedType (primitiveId NeverT) [])),
      (Name "void", TypeSymbol (DefinedType (primitiveId VoidT) [])),
      (Name "Native", TypeSymbol (DefinedType (primitiveId NativeT) [])),
      (Name "List", TypeConsSymbol (primitiveId ListT)),
      (Name "Map", TypeConsSymbol (primitiveId MapT)),
      (Name "Set", TypeConsSymbol (primitiveId SetT)),
      (Name "bool", TypeSymbol (DefinedType (enumPrimitiveId BoolT) [])),
      (Name "true", EnumVariantSymbol (enumPrimitiveId BoolT) (Name "true")),
      (Name "false", EnumVariantSymbol (enumPrimitiveId BoolT) (Name "false")),
      (Name "Result", TypeConsSymbol (enumPrimitiveId ResultT)),
      (Name "Ok", EnumVariantSymbol (enumPrimitiveId ResultT) (Name "Ok")),
      (Name "Err", EnumVariantSymbol (enumPrimitiveId ResultT) (Name "Err")),
      (Name "Option", TypeConsSymbol (enumPrimitiveId OptionT)),
      (Name "Some", EnumVariantSymbol (enumPrimitiveId OptionT) (Name "Some")),
      (Name "None", EnumVariantSymbol (enumPrimitiveId OptionT) (Name "None")),
      (Name "hash", TraitSymbol (traitPrimitiveId HashTrait)),
      (Name "eq", TraitSymbol (traitPrimitiveId EqTrait))
    ]

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable initialSymbolTable emptyBoundTraits

newScopeGroup :: SymbolScopeGroup
newScopeGroup = SymbolScopeGroup (SymbolTable initialSymbolTable emptyBoundTraits) []

-- | The main typechecking context.
--
-- Contains information about traits, types, symbols, parsed modules, and
-- various state relating to the current position of the typechecker.
data TypecheckCtx = TypecheckCtx
  { _symbolScopes :: SymbolScopeGroup,
    _typeTable :: TypeTable,
    _genTypeVarBounds :: HM.HashMap Int BoundTraits,
    _symbolCounter :: Int,
    _traitCounter :: Int,
    _inLoop :: Bool,
    _funcRetType :: Maybe Type,
    _retOnce :: Bool,
    _runtimeEnumCounter :: Int,
    _heapRefCounter :: Int,
    _initialSymbols :: SymbolScopeGroup,
    _typeVarCounter :: Int,
    _typeVarCtx :: HM.HashMap Name Type,
    _currentNamespace :: FilePath,
    _modules :: HM.HashMap FilePath PB.ParsedModule,
    _funcName :: Maybe String,
    _loadedNamespaces :: HM.HashMap FilePath SymbolScopeGroup
  }
  deriving (Show)

-- | The typechecking monad.
--
--  Provides state, error handling and IO.
type Typecheck a = ExceptT HashError (StateT TypecheckCtx IO) a

runTypecheck :: Typecheck a -> TypecheckCtx -> IO (Either HashError a, TypecheckCtx)
runTypecheck = runStateT . runExceptT

-- | Max 100 free runtime symbols
freeRuntimeSymbolBound :: Int
freeRuntimeSymbolBound = 100

-- | Max 100 free runtime enum variants
freeRuntimeEnumVariantBound :: Int
freeRuntimeEnumVariantBound = 100

-- | Get a free symbol counter
freeRuntimeSymbol :: Int -> RB.Name
freeRuntimeSymbol i | i >= freeRuntimeSymbolBound = internalPanicPure "exceeded free symbols"
freeRuntimeSymbol i = RB.Name $ i + HM.size initialSymbolTable

-- | Get a free symbol counter
freeRuntimeEnumVariant :: Int -> RB.EnumVariant
freeRuntimeEnumVariant i | i >= freeRuntimeEnumVariantBound = internalPanicPure "exceeded free symbols"
freeRuntimeEnumVariant i = RB.EnumVariant i

-- | Create a new typechecking context at the given namespace and with the
-- given initial symbol table and module graph.
newTypecheckCtx :: FilePath -> SymbolTable -> HM.HashMap FilePath PB.ParsedModule -> TypecheckCtx
newTypecheckCtx fp initialSyms mods =
  TypecheckCtx
    { _symbolScopes = SymbolScopeGroup initialSyms [SymbolTable initialSymbolTable emptyBoundTraits],
      _genTypeVarBounds = HM.empty,
      _typeTable =
        TypeTable
          ( HM.fromList $
              map
                (\p -> (primitiveId p, primitiveT p))
                [minBound .. maxBound]
                ++ map
                  (\p -> (enumPrimitiveId p, enumPrimitiveT p))
                  [minBound .. maxBound]
          )
          (HM.fromList $ map (\t -> (traitPrimitiveId t, traitPrimitive t)) [minBound .. maxBound]),
      _symbolCounter = HM.size initialSymbolTable + freeRuntimeSymbolBound,
      _typeVarCounter = 0,
      _runtimeEnumCounter = freeRuntimeEnumVariantBound,
      _heapRefCounter = HM.size intrinsicMap,
      _inLoop = False,
      _funcRetType = Nothing,
      _retOnce = False,
      _funcName = Nothing,
      _typeVarCtx = HM.empty,
      _modules = mods,
      _initialSymbols = SymbolScopeGroup initialSyms [SymbolTable initialSymbolTable emptyBoundTraits],
      _traitCounter = fromEnum (maxBound :: TraitPrimitive),
      _currentNamespace = fp,
      _loadedNamespaces = HM.empty
    }

-- | Represents a Hindley-Milner type substitition, used for type inference.
newtype Substitution = Substitution {_subMap :: HM.HashMap Int Type}
  deriving (Show)

-- Lenses:
makeLenses ''StructType
makeLenses ''EnumType
makeLenses ''TypeTable
makeLenses ''FunctionType
makeLenses ''PrimitiveType
makeLenses ''Trait
makeLenses ''Bound
makeLenses ''ImplBound
makeLenses ''TraitBound
makeLenses ''BoundTraits
makeLenses ''Symbol
makeLenses ''TypecheckCtx
makeLenses ''SymbolScopeGroup
makeLenses ''Substitution
makeLenses ''SymbolTable
makeLenses ''StructField

-- | Throw a type error with the given message and position.
typeError :: PB.AstNode b -> String -> Typecheck a
typeError a msg = throwE (TypeError msg a {PB.body = ()})

-- | Get the type definition for a given type ID.
lookupTypeDef :: TypeId -> Typecheck TypeDef
lookupTypeDef tId = do
  maybeTypeDef <- gets (views (typeTable . tableData) (HM.lookup tId))
  case maybeTypeDef of
    Just t -> return t
    _ -> internalPanicPure "Unknown type id encountered."

-- | Modify the trait definition for a given trait ID.
modifyTraitDef :: TraitId -> (Trait -> Trait) -> Typecheck ()
modifyTraitDef tId f = do
  modify $
    over
      (typeTable . traitData)
      ( \tab ->
          if HM.member tId tab
            then HM.adjust f tId tab
            else internalPanicPure "Unknown trait id encountered."
      )

-- | Lookup the trait definition for a given trait ID.
lookupTraitDef :: TraitId -> Typecheck Trait
lookupTraitDef tId = do
  maybeTypeDef <- gets (views (typeTable . traitData) (HM.lookup tId))
  case maybeTypeDef of
    Just t -> return t
    _ -> internalPanicPure "Unknown trait id encountered."

-- | Lookup the symbol for a given name.
lookupName :: PB.AstNode PB.Name -> Typecheck Symbol
lookupName node = do
  curr <- gets . view $ symbolScopes . currSymbolTable
  rest <- gets . view $ symbolScopes . restSymbolTables
  lookupName' node (curr : rest)

-- | Max lookup names when using auto completion.
maxResults :: Int
maxResults = 50

-- | Go up the scope group and find as many symbols that are prefixed
-- | with the given prefix and return those. The current internal limit
-- | to find symbols is 50.
lookupNameByPrefix :: String -> Typecheck [String]
lookupNameByPrefix n = do
  curr <- gets . view $ symbolScopes . currSymbolTable
  rest <- gets . view $ symbolScopes . restSymbolTables
  lookupNameByPrefix' (curr : rest) (Name n) maxResults

lookupNameByPrefix' :: [SymbolTable] -> PB.Name -> Int -> Typecheck [String]
lookupNameByPrefix' [] _ _ = return []
lookupNameByPrefix' _ _ limit | limit <= 0 = return []
lookupNameByPrefix' (sym : syms) n@(Name name) limit = do
  let symData = view symbolData sym
  let k = map (\(Name x) -> x) (filter (\(Name x) -> name `isPrefixOf` x) (HM.keys symData))
  lookupNameByPrefix' syms n (limit - length k) >>= \x -> return (k ++ x)

-- | Same as lookupName, but does not throw on failure.
maybeLookupName :: PB.AstNode PB.Name -> Typecheck (Maybe Symbol)
maybeLookupName node = do
  curr <- gets . view $ symbolScopes . currSymbolTable
  rest <- gets . view $ symbolScopes . restSymbolTables
  return $ maybeLookupName' node (curr : rest)

maybeLookupName' :: PB.AstNode PB.Name -> [SymbolTable] -> Maybe Symbol
maybeLookupName' _ [] = Nothing
maybeLookupName' node@(PB.AstNode n _ _ _) (t : ts) =
  case n `HM.lookup` view symbolData t of
    Just x -> Just x
    Nothing -> maybeLookupName' node ts

lookupName' :: PB.AstNode PB.Name -> [SymbolTable] -> Typecheck Symbol
lookupName' n t =
  case maybeLookupName' n t of
    Nothing -> typeError n $ "Name '" ++ show n ++ "' not in scope."
    Just x -> return x

lookupSymbol' :: PB.AstNode PB.AccessName -> [SymbolTable] -> Typecheck Symbol
lookupSymbol' (PB.AstNode (PB.AccessName []) _ _ _) _ = internalPanicPure "Found empty access name"
lookupSymbol' (PB.AstNode (PB.AccessName [n]) _ _ _) table = lookupName' n table
lookupSymbol' node@(PB.AstNode (PB.AccessName (n : ns)) _ _ _) table = do
  firstSym <- lookupName' n table
  case firstSym of
    TypeSymbol (DefinedType tId []) -> do
      typeDef <- lookupTypeDef tId
      case typeDef of
        NamespaceT _ newTable -> lookupSymbol' (A.astNodeAt node (PB.AccessName ns)) [newTable]
        _ -> isNotNamespace
    _ -> isNotNamespace
  where
    isNotNamespace = typeError n $ "Name '" ++ show n ++ "' is not a namespace."

-- | Lookup a symbol by a given access name.
--
-- This can also traverse namespaces.
lookupSymbol :: PB.AstNode PB.AccessName -> Typecheck Symbol
lookupSymbol node = do
  curr <- gets . view $ symbolScopes . currSymbolTable
  rest <- gets . view $ symbolScopes . restSymbolTables
  lookupSymbol' node (curr : rest)

-- | Add a symbol.
addSymbol :: PB.Name -> Symbol -> Typecheck ()
addSymbol n s =
  modify $ over (symbolScopes . currSymbolTable . symbolData) (HM.insert n s)

-- | Create a new runtime symbol.
newRuntimeSymbol :: Typecheck RB.Name
newRuntimeSymbol = RB.Name <$> newSymbolCode

-- | Add a ValueSymbol and return it.
addValueName :: PB.Name -> Type -> Typecheck Symbol
addValueName n t = do
  code <- newSymbolCode
  let s = ValueSymbol code t
  modify $ over (symbolScopes . currSymbolTable . symbolData) (HM.insert n s)
  return s

-- | Define a new type and return its type ID.
addType :: TypeDef -> Typecheck TypeId
addType def = do
  modify $ typeVarCounter +~ 1
  c <- gets $ view typeVarCounter
  modify $ over (typeTable . tableData) (HM.insert (TypeId c) def)
  return (TypeId c)

-- | Make sure that the given name is not defined.
ensureNotDefined :: PB.AstNode PB.Name -> Typecheck ()
ensureNotDefined n =
  maybeLookupName n >>= \case
    Nothing -> return ()
    Just _ -> typeError n $ "Name '" ++ show n ++ "' is already defined."

-- | Add a type constructor symbol.
addTypeConsSymbol :: PB.Name -> TypeId -> Typecheck Symbol
addTypeConsSymbol n t = do
  let s = TypeConsSymbol t
  modify $ over (symbolScopes . currSymbolTable . symbolData) (HM.insert n s)
  return s

-- | Add a type symbol.
addTypeSymbol :: PB.Name -> Type -> Typecheck Symbol
addTypeSymbol n t = do
  let s = TypeSymbol t
  modify $ over (symbolScopes . currSymbolTable . symbolData) (HM.insert n s)
  return s

-- | Traverse the symbol tables and modify if the provided function matches.
modifySymbolTableIf :: (SymbolTable -> Maybe (SymbolTable -> SymbolTable)) -> SymbolScopeGroup -> SymbolScopeGroup
modifySymbolTableIf f (SymbolScopeGroup a []) = case f a of
  Nothing -> SymbolScopeGroup a []
  Just g -> SymbolScopeGroup (g a) []
modifySymbolTableIf f (SymbolScopeGroup a (x : xs)) = case f a of
  Nothing -> let (SymbolScopeGroup m ms) = modifySymbolTableIf f (SymbolScopeGroup x xs) in SymbolScopeGroup a (m : ms)
  Just g -> SymbolScopeGroup (g a) (x : xs)

-- | Define a new trait and return its ID.
addTrait :: Trait -> Typecheck TraitId
addTrait t = do
  modify $ traitCounter +~ 1
  tId <- gets (TraitId . view traitCounter)
  modify $ over (typeTable . traitData) (HM.insert tId t)
  return tId

-- | Add type variable bounds to state.
addTypeVarBounds :: BoundTraits -> Typecheck ()
addTypeVarBounds bs = do
  modify $
    over
      (symbolScopes . currSymbolTable . typeVarBounds . boundTraitsData)
      (++ view boundTraitsData bs)

-- | Add generated type variable bounds to state.
addGenTypeVarBounds :: Int -> BoundTraits -> Typecheck ()
addGenTypeVarBounds tId bs = do
  modify $
    over
      genTypeVarBounds
      (HM.insertWith (\(BoundTraits old) (BoundTraits new) -> BoundTraits (old ++ new)) tId bs)

-- | Get generated type variable bounds for a given generated type variable ID.
getGenTypeVarBounds :: Int -> Typecheck BoundTraits
getGenTypeVarBounds tId = gets $ views genTypeVarBounds (HM.lookupDefault emptyBoundTraits tId)

-- | Get all type variable bounds.
getTypeVarBounds :: Typecheck BoundTraits
getTypeVarBounds = gets $ view (symbolScopes . currSymbolTable . typeVarBounds)

-- | Add a trait symbol.
addTraitSymbol :: PB.Name -> TraitId -> Typecheck Symbol
addTraitSymbol n tId = do
  let s = TraitSymbol tId
  modify $ over (symbolScopes . currSymbolTable . symbolData) (HM.insert n s)
  return s

-- | Create a new generated type variable.
newTypeVar :: Typecheck Type
newTypeVar = do
  modify $ typeVarCounter +~ 1
  c <- gets $ view typeVarCounter
  return $ GenTypeVar c

-- | Create a new runtime enum variant.
--
-- Runs every time the user defines an enum variant.
newRuntimeEnumVariant :: Typecheck RB.EnumVariant
newRuntimeEnumVariant = do
  modify $ runtimeEnumCounter +~ 1
  gets (RB.EnumVariant . view runtimeEnumCounter)

-- | Add an enum variant symbol to the state.
addEnumVarSymbol :: PB.Name -> TypeId -> Typecheck ()
addEnumVarSymbol name tId = do
  let s = EnumVariantSymbol tId name
  modify $ over (symbolScopes . currSymbolTable . symbolData) (HM.insert name s)

-- | Create a new symbol code (for runtime symbol indices).
newSymbolCode :: Typecheck Int
newSymbolCode = do
  modify $ symbolCounter +~ 1
  gets $ view symbolCounter

-- | Error about mismatching types.
mismatchError :: PB.AstNode b -> Type -> Type -> Typecheck a
mismatchError node a b = typeError node $ "Mismatching types. Expected " ++ show a ++ " but got " ++ show b ++ "."

-- | Implementation of `Show` for type arguments.
showTypeArgs :: [Type] -> Typecheck String
showTypeArgs [] = return ""
showTypeArgs ts = do
  tsShow <- mapM showType ts
  return $ "<" ++ intercalate ", " tsShow ++ ">"

-- | Implementation of `Show` for type primitives.
showPrimitive :: Primitive -> [Type] -> Typecheck String
showPrimitive prim types = case (prim, types) of
  (VoidT, []) -> return "void"
  (NeverT, []) -> return "never"
  (ListT, [t]) -> do
    tShow <- showType t
    return $ "[" ++ tShow ++ "]"
  (MapT, [k, v]) -> do
    kShow <- showType k
    vShow <- showType v
    return $ "{" ++ kShow ++ ":" ++ vShow ++ "}"
  (SetT, [t]) -> do
    tShow <- showType t
    return $ "{" ++ tShow ++ "}"
  (StrT, []) -> return "str"
  (CharT, []) -> return "char"
  (FloatT, []) -> return "float"
  (IntT, []) -> return "int"
  _ -> internalPanicPure "unexpected primitive"

-- | Implementation of `Show` for user type definitions.
showTypeDef :: [Type] -> TypeDef -> Typecheck String
showTypeDef types = \case
  StructT (StructType (Name n) _ _) -> (n ++) <$> showTypeArgs types
  EnumT (EnumType (Name n) _ _) -> (n ++) <$> showTypeArgs types
  NamespaceT mod _ -> return $ "<namespace of '" ++ mod ++ "'>"
  PrimitiveT (PrimitiveType prim _) -> showPrimitive prim types

-- | Implementation of `Show` for all types.
showType :: Type -> Typecheck String
showType = \case
  DefinedType tId types -> lookupTypeDef tId >>= showTypeDef types
  TypeVar (Name n) -> return n
  GenTypeVar c -> return $ "G" ++ show c
  ExistentialT -> return "?"
  FunctionT (FunctionType args ret _) -> do
    argsShow <- mapM showType args
    retShow <- showType ret
    return $ "(" ++ intercalate ", " argsShow ++ ") => " ++ retShow
  TupleT ts -> do
    tsShow <- mapM showType ts
    return $ "(" ++ intercalate ", " tsShow ++ ")"

-- | Add a symbol table to the given SymbolScopeGroup.
addSymbolTable :: SymbolTable -> SymbolScopeGroup -> SymbolScopeGroup
addSymbolTable (SymbolTable d (BoundTraits t)) (SymbolScopeGroup prev xs) =
  SymbolScopeGroup
    ( SymbolTable
        d
        (BoundTraits $ t ++ view (typeVarBounds . boundTraitsData) prev)
    )
    (prev : xs)

-- | Create a new a symbol table and add it to the given SymbolScopeGroup.
newSymbolTable :: SymbolScopeGroup -> SymbolScopeGroup
newSymbolTable (SymbolScopeGroup prev xs) = SymbolScopeGroup (SymbolTable HM.empty (view typeVarBounds prev)) (prev : xs)

-- | Remove the last symbol table from the given SymbolScopeGroup.
popSymbolTable :: SymbolScopeGroup -> SymbolScopeGroup
popSymbolTable (SymbolScopeGroup _ []) = internalPanicPure "Attempting to evict empty symbol scope group"
popSymbolTable (SymbolScopeGroup _ (x : xs)) = SymbolScopeGroup x xs

-- | Unwrap the given instruction, potentially throwing an error.
{-# INLINE unwrapIns #-}
unwrapIns :: Maybe RB.Instruction -> RB.Instruction
unwrapIns = \case
  Just i -> i
  Nothing -> internalPanicPure "expected instructon to be emitted"
