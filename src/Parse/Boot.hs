{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Hash AST definitions and parsers.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Parse.Boot where

import Control.Lens (makeLenses)
import Control.Monad.State (State)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Set.Ordered (OSet)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as M

-- | Represents an abstract syntax tree node.
--
-- Contains an inner type, as well as begin and end positions in the input.
data AstNode a = AstNode {body :: a, offsetBegin :: Int, offsetEnd :: Int, filename :: String}
  deriving (Eq, Generic)

instance Hashable a => Hashable (AstNode a)

-- | The 'Show' implementation for a parsed 'AstNode'. Since this is used at later
-- | stages to report typing errors, this needs a specialised show method.
instance Show a => Show (AstNode a) where
  show (AstNode b _ _ _) = show b

-- astShowHelper :: Show a => Bool -> AstNode a -> String
-- astShowHelper False (AstNode b begin end _) = "astNode " ++ show (begin + 1) ++ " " ++ show (end - begin) ++ " (" ++ show b ++ ")"
-- astShowHelper True node = "(" ++ astShowHelper False node ++ ")"

-- instance Show a => Show (AstNode a) where
--   show = astShowHelper True

-- instance {-# OVERLAPS #-} Show a => Show [AstNode a] where
--   show x = "[" ++ intercalate ", " (map (astShowHelper False) x) ++ "]"

-- | Enum representing either a Set or Tup type, used to shorten parsing
data BrackettedType = Set | Tuple
  deriving (Show, Eq)

-- | Name of an identifier.
newtype Name = Name String
  deriving (Eq, Generic)

-- | The 'Show' implementation for a parsed 'Name'. Since this is used at later
-- | stages to report typing errors, this needs a specialised show method.
instance Show Name where
  show (Name x) = x

instance Hashable Name

-- | Name which might be nested within namespaces (e.g. a::b::c) where 'a', 'b' are
-- | namespaces, and 'c' is the actual name.
newtype AccessName = AccessName [AstNode Name]
  deriving (Eq)

-- | The 'Show' implementation for a parsed 'AccessName'. Since this is used at later
-- | stages to report typing errors, this needs a specialised show method.
instance Show AccessName where
  show (AccessName xs) = intercalate "::" (map show xs)

-- | An AST representation for a type.
data Type
  = -- | The type is prefixed by an 'AccessName' and then the actual type, for example
    -- | 'math::Complex'
    NamedType (AstNode AccessName) [AstNode Type]
  | -- | A type variable
    TypeVar (AstNode Name)
  | -- | The function tyoe, represented by a zero or more arguments and a single return type.
    FunctionType [AstNode Type] (AstNode Type)
  | -- | The type '?', essentially where the type does not matter and will be inferred
    ExistentialType
  | -- | The this is the '_' type, that will be inferred at the typechecking stage
    InferType
  deriving (Show, Eq)

-- | AST representation of an intrinsic key which is essentially a call to an
-- | internally defined compiler function.
newtype IntrinsicKey = IntrinsicKey String
  deriving (Show, Eq)

-- | AST representation of a Trait bound. A trait bound can be represented as
-- | an 'AccessName' followed by angle brackets with one or more generic 'Type'
-- | arguments.
data TraitBound = TraitBound (AstNode AccessName) [AstNode Type]
  deriving (Show, Eq)

-- | AST representation of a 'ForAll' type construct.
data ForAll = ForAll [AstNode Type] [AstNode TraitBound]
  deriving (Show, Eq)

-- | AST representation for a struct entry, which will include a name, type, and potentially
-- | an expression which represents a default value for the field. For example:
-- |
-- | struct Name = {
-- |    field: int = 13;
-- |    ^^^^^  ^^^   ^^──┐
-- |     Name  type     the default value
-- |     ...
-- | }
data StructEntry = StructEntry (AstNode Name) (Maybe (AstNode Type)) (Maybe (AstNode Expression))
  deriving (Show, Eq)

-- | AST representatio of a struct which includes the name of the struct with a
-- | ForAll to specify any bounds or and generic arguments to the struct, with
-- | zero or more struct fields. An example for a struct would be:
-- |
-- | struct Name = <T,Q> where eq<T> => { ... };
-- |        ^^^^    ^──────^^─┬──^        ^^^
-- | Name of struct        For all       fields
data Struct = Struct (AstNode Name) (AstNode ForAll) [AstNode StructEntry]
  deriving (Show, Eq)

-- | AST representation for a enum field, which will include a name, type.
-- | An example enum field would be:
-- |
-- | enum Name = {
-- |    Field(int, float)
-- |    ^^^^^  ^^^   ^^──┐
-- |     Name  field types
-- |     ...
-- | }
data EnumEntry = EnumEntry (AstNode Name) [AstNode Type]
  deriving (Show, Eq)

-- | AST representation for an enum, An enum is constrcuted by a the keyword 'enum'
-- | follwed by an idenfier name, a for-all declaratation, followed by some enum fields.
-- | An enumeration can be made of zero or more enum fields, an example declaration of
-- | For example, a declaration of an enun would be:
-- |
-- | enum Name = <T,Q> where eq<T> => { ... };
-- |      ^^^^    ^──────^^─┬──^        ^^^
-- | Name of enum        For all       fields
data Enumeration = Enumeration (AstNode Name) (AstNode ForAll) [AstNode EnumEntry]
  deriving (Show, Eq)

-- | AST representation of a trait statement. A trait statement
-- | is essentially a function with no body, with a for-all node and
-- | some genetic type arguments. For example,
-- |
-- | trait eq<T> = (T, T) => bool;
-- |     ┌─^^ ^─┐   ^─ ─ ─ ─ ─ ─ ─ ┐
-- |   name   Generic type args    Function type definition
data Trait = Trait (AstNode Name) (AstNode ForAll) (AstNode Type)
  deriving (Show, Eq)

-- | AST representation of a map entry
data MapEntry = MapEntry (AstNode Expression) (AstNode Expression)
  deriving (Show, Eq)

-- | AST representation for a literal within the language.  For more information
-- | on the syntax of various literal constructs, check out the wiki page at:
-- | https://feds01.github.io/hash/types.html
data Literal
  = -- | String literal type, zero or more chars between double quoutes (").
    StrLiteral String
  | -- | Char literal type, single character between single quoutes (').
    CharLiteral Char
  | -- | Integer literal, supports the spaced digit seperators. For example, writing
    -- | an int literal like '1_000_000' would translate into '1000000'
    IntLiteral Integer
  | -- | Float literal, supports standard scienitific notation.
    FloatLiteral Double
  | -- | Set literal, zero or more expressions seperated by commas within
    -- | '{' braces '}'. Supports trailing comma.
    SetLiteral [AstNode Expression]
  | -- | Map literal, zero or more 'MapEntry' AST nodes seperated by commas within
    -- | '{' braces '}'. Supports trailing comma.
    MapLiteral [AstNode MapEntry]
  | -- | List literal, zero or more expressions seperated by commas within
    -- | '[' square brackets ']'. Supports trailing comma.
    ListLiteral [AstNode Expression]
  | -- | Tuple literal, zero or more expressions seperated by commas within
    -- | '(' parenthesees ')'. Supports trailing comma.
    TupleLiteral [AstNode Expression]
  | -- | Struct literal, an access name with zero or more 'StructField' AST nodes, which
    -- | translated into using a HashMap.
    StructLiteral (AstNode Type) (HM.HashMap (AstNode Name) (AstNode Expression))
  | -- | Function literal, zero or more of function argumemts, optional return type and a function body.
    FunctionLiteral [AstNode FunctionParam] (Maybe (AstNode Type)) (AstNode Expression)
  deriving (Show, Eq)

-- | AST representation of Function parameters when specifying the types of arguments
-- | on the righthand side of the (=) equals operator. Function parameters must have a name
-- | and can specify a type of the parameter. If the type is not specified, it will be
-- | inferred at type checking, For example a func parameter is:
-- |
-- | let x = (s: int) => ...
-- |          ^^^^^^
-- |        this is the function param
-- |
data FunctionParam = FunctionParam (AstNode Name) (Maybe (AstNode Type))
  deriving (Show, Eq)

-- | AST representation for a funtction call with the potential for specifying
-- | generic arguments and a list of actual arguments to the function. For example:
-- |
-- | function_call<int, str>(some, arguments, for, func);
-- |              ^^^^^^^^^^ ^---------^-------^-----^
-- |           Type arguments to call        Function call arguments
data FunctionCallArgs = FunctionCallArgs (Maybe [AstNode Type]) [AstNode Expression] String
  deriving (Show)

instance Eq FunctionCallArgs where
  (==) (FunctionCallArgs x1 x2 _) (FunctionCallArgs y1 y2 _) = x1 == y1 && x2 == y2

-- | Data type representing either an '&&' or '||' operatpr. This a data type to
-- | solve precendence within the language.
data LogicalOp = LogicalAndOp | LogicalOrOp
  deriving (Show, Eq)

-- | AST represenation for an Expression which is a single part of a statement.
-- | An 'Expression' can be with other 'Expresion's to be combined into a statement.
data Expression
  = FunctionCall (AstNode Expression) (AstNode FunctionCallArgs)
  | IntrinsicExpr IntrinsicKey
  | LogicalOp LogicalOp (AstNode Expression) (AstNode Expression)
  | Variable (AstNode AccessName)
  | PropertyAccess (AstNode Expression) (AstNode Name)
  | LiteralExpr (AstNode Literal)
  | TypedExpression (AstNode Expression) (AstNode Type)
  | BlockExpr (AstNode Block) -- should not return void
  | Import (AstNode Literal) -- arg is path
  deriving (Show, Eq)

-- | AST representation for a statement. A statement is a generic representation
-- | of some AST construct within the language.
data Statement
  = ExprStatement (AstNode Expression) -- should return void
  | Return (Maybe (AstNode Expression))
  | BlockStatement (AstNode Block) -- should return void
  | -- | Break keyword statement
    Break
  | -- | Continue keyword statement
    Continue
  | -- | Let keyword statement, a destructuring pattern, potential for-all statement, optional
    -- | type definition and a potential definition of the right hand side. For example:
    -- |
    -- | let some_var<int>: float = ...
    -- |       ^^^^  ^^^^^   ^^^     ^^^─────┐
    -- |  pattern   for-all  type of let    the right hand-side expr
    Let (AstNode Pattern) (Maybe (AstNode ForAll)) (Maybe (AstNode Type)) (Maybe (AstNode Expression))
  | -- | Assign statement (which can be some 'PropertyAccess') and the definition of right-hand side which is some expression
    Assign (AstNode Expression) (AstNode Expression)
  | -- | Definition of a struct
    StructDef (AstNode Struct)
  | -- | Definition of a enum
    EnumDef (AstNode Enumeration)
  | -- | Definition of a trait
    TraitDef (AstNode Trait)
  deriving (Show, Eq)

-- | AST represenration for a destructuring entry which can be anything
data DestructuringEntry = DestructuringEntry (AstNode Name) (AstNode Pattern)
  deriving (Show, Eq)

-- | AST representation for a destructing construct which could be used for either
-- | a namespce, struct, enum, tuple entru.
newtype Destructuring = Destructuring [AstNode DestructuringEntry]
  deriving (Show, Eq)

-- | AST representation of a Pattern construct. Pattern constructs can be made
-- | of any kind of literal expresion, or 'DestrcuturingEntry's to assign some
-- | pattern to a variable name. Pattern constructs are very similar to ones that
-- | are present within Haskell, Rust and some other languages. For more information
-- | about patterns, check out the wiki page: https://feds01.github.io/hash/pattern-matching.html
data Pattern
  = -- | Pattern to match some enum field, such as Name(a, b)
    PatternEnum (AstNode AccessName) [AstNode Pattern]
  | -- | Pattern to match a struct literal with fields as destructuring entries
    PatternStruct (AstNode AccessName) [AstNode DestructuringEntry]
  | -- | Namespace pattern used to when access members from a namespace or a map
    -- | because they use the same syntax.
    PatternNamespace [AstNode DestructuringEntry]
  | -- | Tuple pattern matching construct, some number of patterns surrounded by parenthesees.
    PatternTuple [AstNode Pattern]
  | -- | Pattern literal covers any kind of 'Literal' that can be matched.
    PatternLiteral (AstNode Literal)
  | -- | PatternOr construct is used to combine multiple patterns into one. For example
    -- | when you want one match case to execute when x is either `4` or `5`:
    -- |
    -- | match x {
    -- |    4 | 5 => ...
    -- |      ^
    -- |   PatternOr, execute when x is 4 or 5.
    -- |
    PatternOr (AstNode Pattern) (AstNode Pattern)
  | -- | PatternIf construct is very similar to the 'PatternOr' construct but it's
    -- | logical binding operation is AND instead of OR.
    PatternIf (AstNode Pattern) (AstNode Expression)
  | -- | Bind the value to some identifier.
    PatternBinding (AstNode Name)
  | -- | Default, ignore case. This matches any kind of pattern.
    PatternIgnore
  deriving (Show, Eq)

-- | AST representation for a 'Match' statement case. Essentially, a 'Pattern',
-- | separated by a '=>' (arrow operator) and followed by some expression.
data MatchCase = MatchCase (AstNode Pattern) (AstNode Expression)
  deriving (Show, Eq)

-- | AST representation of a block, which can either be a 'Match' statement,
-- | a 'Loop' statement or a 'Body' statement which is a list of 'Statements'
-- | followed by a potential return.
data Block
  = Match (AstNode Expression) [AstNode MatchCase]
  | Loop (AstNode Block)
  | Body [AstNode Statement] (Maybe (AstNode Expression))
  deriving (Show, Eq)

data REPLBlock = REPLBlock [AstNode Statement] (Maybe (AstNode Expression))

-- | AST representation of a module, just a list of statements.
newtype Module = Module [AstNode Statement]
  deriving (Show, Eq)

-- | The parser to use for when generating an AST
type Parser = M.ParsecT Void Text (State ModuleContext)

-- | Module graph represents the module dependencies for a given module.
newtype ModuleGraph = ModuleGraph {modGraphMap :: HM.HashMap FilePath (HS.HashSet FilePath)}
  deriving (Show)

-- | ParsedModule represents the parsed module including the AST node, file path on disk, and the original source.
data ParsedModule = ParsedModule
  { _moduleNode :: AstNode Module,
    _modulePath :: FilePath,
    _realModulePath :: FilePath,
    _srcText :: Text
  }
  deriving (Generic, Show)

-- | Parsing context is generated by the parser when parsing a module or a
-- | or any kind of input.
data ParsingCtx = ParsingCtx
  { _modules :: HM.HashMap FilePath ParsedModule,
    _alreadyParsedPaths :: HS.HashSet FilePath,
    _moduleGraph :: ModuleGraph
  }
  deriving (Generic, Show)

-- | Module context represents the dependecies of a given module
newtype ModuleContext = ModuleContext
  { _moduleDeps :: OSet FilePath
  }
  deriving (Generic, Show)

makeLenses ''ModuleContext
makeLenses ''ParsedModule
makeLenses ''ParsingCtx
