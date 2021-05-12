{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Hash compiler type definitions.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Runtime.Boot where

import Control.Error (ExceptT, runExceptT)
import Control.Lens (makeLenses, view)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (StateT (runStateT), gets)
import Data.Dynamic (Dynamic)
import Data.Either (fromRight)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (..))
import Data.List (group)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Panic (internalPanic', internalPanicPure)
import System.Exit (exitFailure)

-- ############################################################
--                        Heap
-- ############################################################

-- | The typeclass for objects that have a calculatable size.
--
-- This is in order to keep track of the heap size, for garbage collection.
class Sized a where
  -- | Get the size of the given value in approximate units of `CopyValue`.
  sizeof :: a -> Int

-- | The typeclass for objects that contain other references.
--
-- This is for garbage collection.
class RefTrace a where
  -- | Get all the heap references contained in the given value.
  refTrace :: a -> HS.HashSet Ref

-- | Represents a reference into the heap.
--
-- Depending on the architecture, you can have 2^63 references within the VM in
-- a single running instance.
newtype Ref = Ref Int
  deriving (Show, Eq, Generic)

instance Hashable Ref

-- | Represents a name (reference into the stack).
newtype Name = Name Int
  deriving (Show, Eq, Generic)

instance Hashable Name

-- | Represents a hashed integer, used internally as a key for maps and sets.
newtype Hashed = Hashed Integer
  deriving (Show, Eq, Generic)

instance Hashable Hashed

-- | Represents the running program data.
data ProgramCtx = ProgramCtx
  { -- | The program heap.
    _heap :: Heap,
    -- | The program stack.
    _stack :: Stack,
    -- | We track this to avoid getting the list length each time, because haskell is
    -- | terrible with lists
    _currentStackSize :: Int,
    -- | Maximum stack size for the current execution
    _maxStackSize :: Int
  }
  deriving (Show)

-- | The `Execution` environment, which allows access to `ProgramCtx` and `IO`.
type Execution = StateT ProgramCtx IO

-- | Type synonym for intrinsic function
type IntrinsicFunc = [CopyValue] -> Execution CopyValue

-- | Represents a native function.
--
-- This stores a Haskell function that transforms some arguments (typed as
-- `CopyValue`) into another `CopyValue`.
newtype NativeFunction = NativeFunction IntrinsicFunc

instance Show NativeFunction where
  show (NativeFunction _) = "() => {<native>}"

-- | Represents a function value in the heap.
--
-- This is defined here instead of in Heap, to break circular dependency
-- issues.
data FunctionValue = FunctionValue
  { -- | The name of the function.
    _fnValName :: String,
    -- | The scope group that needs to be available when calling the function.
    _fnContext :: ScopeGroup,
    -- | The names to bind the function arguments to.
    _argNames :: [Name],
    -- | The instruction that executes the function.
    _fnInner :: Instruction
  }
  deriving (Show, Eq)

instance RefTrace FunctionValue where
  refTrace (FunctionValue _ ctx _ _) = refTrace ctx

-- | Represents a variant of an enum.
--
-- Each enum variant in the program is given a unique integer during
-- typechecking.
newtype EnumVariant = EnumVariant Int
  deriving (Show, Eq)

-- | Represents a value which can be stored on the stack.
data CopyValue
  = -- | Reference to the heap.
    RefValue Ref
  | -- | An integer.
    IntV Integer
  | -- | A float.
    FloatV Double
  | -- | A character.
    CharV Char
  | -- | A tuple.
    TupleV (V.Vector CopyValue)
  | -- | An enum variant.
    EnumV EnumVariant (Maybe Ref)
  | -- | Void (empty value).
    VoidV
  | -- Uninitialised variable
    UnInitV
  deriving (Show, Eq)

-- | 'Sized' typeclass implementation for 'CopyValue's. Given that haskell ADTs
-- | are of unknon size ( at least they are the same size), there is no good way
-- | to get actual sizes and hence we hueristacally assume they are all of size 1,
-- | except tuples that are of the size of how many members they have.
instance Sized CopyValue where
  sizeof = \case
    TupleV entries -> 1 + sum (V.map sizeof entries)
    _ -> 1

-- | Implementation for 'RefTrace' typeclass, for the 'CopyValue'.
instance RefTrace CopyValue where
  refTrace = \case
    RefValue ref -> HS.singleton ref
    EnumV _ (Just entryRef) -> HS.singleton entryRef
    TupleV entries -> V.foldr (\x acc -> acc `HS.union` refTrace x) HS.empty entries
    _ -> HS.empty

-- | Represents a value which can be stored on the heap.
data HeapValue
  = -- | A list value.
    ListV (S.Seq CopyValue)
  | -- | A struct value.
    StructV (V.Vector CopyValue)
  | -- | A map (hashmap).
    MapV (HM.HashMap Hashed [(CopyValue, CopyValue)])
  | -- | A set (hashset).
    SetV (HM.HashMap Hashed [CopyValue])
  | -- | A string.
    StrV Text
  | -- | A native Haskell value. Used by intrinsics to keep state.
    NativeV Dynamic
  | -- | A native function value.
    NativeFnV NativeFunction
  | -- | A function value.
    FunctionV FunctionValue
  | -- | Enum variant arguments.
    EnumArgs (V.Vector CopyValue)
  deriving (Show)

-- | Implementation for Sized typeclass for 'HeapValue' objectsrepresenting the size of each
-- | item that lives on the heap. This is a pure hueristic to immitate sizes within the Haskell
-- | world. Some of the true values might be wildly innacurate(functions and native functions).
instance Sized HeapValue where
  sizeof = \case
    ListV entries -> 1 + sum (fmap sizeof entries)
    StructV entries -> 1 + sum (V.map sizeof entries)
    MapV entries -> 1 + sum (map (\(_, xs) -> sum $ map (\(k, v) -> sizeof k + sizeof v) xs) (HM.toList entries))
    SetV entries -> 1 + sum (map (\(_, k) -> sum $ map sizeof k) (HM.toList entries))
    StrV txt -> 1 + T.length txt
    NativeV _ -> 2
    NativeFnV _ -> 2
    FunctionV _ -> 3 -- @Improvement: maybe we need better estimates for these?
    EnumArgs args -> 1 + sum (V.map sizeof args)

-- | Type class representing the number of references each object that lives on the
-- | heap has.
instance RefTrace HeapValue where
  refTrace = \case
    ListV entries -> HS.unions (map refTrace (toList entries))
    StructV entries -> V.foldr (\x acc -> acc `HS.union` refTrace x) HS.empty entries
    MapV entries -> HS.unions (map (\(_, xs) -> HS.unions $ map (\x -> refTrace (fst x) `HS.union` refTrace (snd x)) xs) (HM.toList entries))
    SetV entries -> HS.unions (map (\(_, xs) -> HS.unions $ map refTrace xs) (HM.toList entries))
    FunctionV f -> refTrace f
    EnumArgs args -> V.foldr (\x acc -> acc `HS.union` refTrace x) HS.empty args
    _ -> HS.empty

-- | An entry into the heap.
data HeapEntry = HeapEntry
  { -- | The value for this entry.
    _heapVal :: HeapValue,
    -- | The size of the value (approximate).
    _valSize :: Int
  }
  deriving (Show)

-- | Represents the program heap.
data Heap = Heap
  { -- | Contains `HeapEntry`s indexed by `Ref` keys.
    _heapData :: HM.HashMap Ref HeapEntry,
    -- | Last `Ref` value, increments every time something is added to the heap.
    _heapCounter :: Int,
    -- | Size of the heap, in approximate units of CopyValue.
    _heapSize :: Int,
    -- | Size that the heap has to grow to before garbage collection is triggered.
    --
    -- This increases to twice the size after garbage collection, every time it
    -- is triggered.
    _maxSizeBeforeGc :: Int
  }
  deriving (Show)

-- | Initial value for `Heap` `_maxSizeBeforeGc`.
initialSizeBeforeGc :: Int
initialSizeBeforeGc = 1

-- | Create an empty heap with zero heap counter.
emptyHeap :: Heap
emptyHeap = Heap HM.empty 0 0 initialSizeBeforeGc

-- | An empty `ProgramCtx`.
emptyProgramCtx :: Int -> ProgramCtx
emptyProgramCtx maxSt =
  ProgramCtx
    { _heap = emptyHeap,
      _stack = emptyStack,
      _currentStackSize = 0,
      _maxStackSize = maxSt
    }

-- | Represents a signal which can change the control flow of the program.
data Signal
  = -- | Break out of the current loop.
    BreakS
  | -- | Continue in the current loop.
    ContinueS
  | -- | Return from the current function.
    ReturnS CopyValue
  deriving (Show, Eq)

-- | The `Execution` environment extended to support signaling with `ExceptT`.
type SignaledExecution a = (ExceptT Signal) Execution a

-- | Represents a pattern.
data PatternR
  = -- | Enum pattern.
    EnumP EnumVariant [PatternR]
  | -- | Or pattern.
    OrP [PatternR]
  | -- | Conditional pattern.
    CondP PatternR Instruction
  | -- | Tuple pattern.
    TupleP [PatternR]
  | -- | Struct pattern.
    StructP [PatternR]
  | -- | Integer literal pattern.
    IntP Integer
  | -- | Float literal pattern.
    FloatP Double
  | -- | Character pattern.
    CharP Char
  | -- | String pattern.
    StrP Text
  | -- | Bind a name to the matched value.
    BindP Name
  | -- | Ignore pattern (match-all).
    IgnoreP
  deriving (Show, Eq)

-- | Represents a virtual machine instruction.
data Instruction
  = -- | A match instruction.
    IMatch
      Instruction
      -- ^ The value to match on.
      [(PatternR, Instruction)]
      -- ^ Match cases.
  | -- | A loop instruction.
    --
    -- Will loop until it receives a signal.
    ILoop Instruction
  | -- | A function call instruction.
    --
    -- This could be a funciton call that is defined within the runtime, or a
    -- native function call.
    IFnCall
      { -- | The function call site, as a string, for backtrace.
        fnCallSite :: String,
        -- | The function to run.
        fn :: Instruction,
        -- | The arguments to bind to the function.
        fnArgs :: [Instruction]
      }
  | -- | A literal value, this is the "identity" instruction.
    ILiteral CopyValue
  | -- | A map literal.
    IMap
      { -- | The hash function to use when hashing the keys.
        mapHashFnRef :: Instruction,
        -- | The map entries.
        mapEntries :: [(Instruction, Instruction)]
      }
  | -- | A set literal.
    ISet
      { -- | The hash function to use when hashing the elements.
        setHashFnRef :: Instruction,
        -- | The set elements.
        setEntries :: [Instruction]
      }
  | -- | A list literal.
    IList (V.Vector Instruction)
  | -- | A tuple literal.
    ITuple (V.Vector Instruction)
  | -- | A string literal.
    IStr Text
  | -- | A char literal.
    IChar Char
  | -- | An enum literal.
    IEnum EnumVariant (V.Vector Instruction)
  | -- | A function definition.
    IFnDef
      { -- | The name of the function, used for backtrace.
        fnDefName :: String,
        -- | The names to bind the function arguments to, so that they are
        -- resolved to values on the stack when the function is called.
        fnDefArgNames :: [Name],
        -- | The function definition as an instruction.
        --
        -- Should throw a `ReturnS` signal to return.
        fnDefInner :: Instruction
      }
  | -- | A struct literal
    --
    -- Each struct field is given an index at typechecking time.
    IStruct (V.Vector Instruction)
  | -- | Bind a name to a value that results from an instruction.
    IBind Name Instruction
  | -- | Bind a pattern to a value that results from an instruction.
    IPatBind PatternR Instruction
  | -- | Perform short-circuiting logical-and.
    ILogicalAnd Instruction Instruction
  | -- | Perform short-circuiting logical-or.
    ILogicalOr Instruction Instruction
  | -- | Set a bound name to a new value.
    INameSet Name Instruction
  | -- | Set a struct field at the given index.
    IPropSet Instruction Int Instruction
  | -- | Get a struct/tuple field at the given index.
    IPropGet Instruction Int
  | -- | Get the value pointed to by the given name.
    INameRef Name
  | -- | Reference to the heap.
    IRef Ref
  | -- | Return a value that results from an instruction. Emits `ReturnS`.
    IReturn Instruction
  | -- | Break out of loop. Emits `BreakS`.
    IBreak
  | -- | Continue in loop. Emits `ContinueS`.
    IContinue
  | -- | Execute an instruction in a new scope.
    IBlock Instruction
  | -- | Execute a series of instructions, returning the last result.
    ISeq [Instruction]
  | -- | Force garbage collection (internal).
    IGC
  deriving (Show, Eq)

-- | Represents a stack frame.
data StackFrame = StackFrame
  { -- | Represents the filename, position and column of the function.
    _callSite :: String,
    -- | The name of the function
    _fnName :: String,
    -- | The scope group for this stack frame.
    _scopeGroup :: ScopeGroup
  }
  deriving (Show, Eq)

instance RefTrace StackFrame where
  refTrace (StackFrame _ _ g) = refTrace g

-- | Represents the program stack.
data Stack = Stack
  { -- | The current (last) stack frame.
    _currFrame :: StackFrame,
    -- | The rest of the stack frames above the current one.
    _frames :: [StackFrame]
  }
  deriving (Show, Eq)

instance RefTrace Stack where
  refTrace (Stack x xs) = refTrace x `HS.union` HS.unions (map refTrace xs)

-- | Create an empty stack.
--
-- Contains a root stack frame, under which all other stack frames will be
-- created.
emptyStack :: Stack
emptyStack = Stack rootStackFrame []

-- | The root stack frame, always present as the first stack frame.
rootStackFrame :: StackFrame
rootStackFrame =
  StackFrame
    { _callSite = "root",
      _fnName = "root",
      _scopeGroup = emptyScopeGroup
    }

-- | Represents a single scope, which is a map between names and values.
newtype Scope = Scope {_symbolMap :: HM.HashMap Name CopyValue}
  deriving (Show, Eq)

instance RefTrace Scope where
  refTrace (Scope m) = HS.unions (map refTrace (HM.elems m))

-- | Represents a set of scopes.
--
-- Name resolution occurs for each scope.
data ScopeGroup = ScopeGroup {_currScope :: Scope, _scopes :: [Scope]}
  deriving (Show, Eq)

instance RefTrace ScopeGroup where
  refTrace (ScopeGroup curr rest) = refTrace curr `HS.union` HS.unions (map refTrace rest)

-- | Create an empty scope group, containing an empty symbol map.
emptyScopeGroup :: ScopeGroup
emptyScopeGroup = ScopeGroup {_currScope = Scope {_symbolMap = HM.empty}, _scopes = []}

-- ############################################################
--                           Lenses
-- ############################################################
-- These need to be defined below everything cause template haskell is stupid.

makeLenses ''ProgramCtx
makeLenses ''HeapEntry
makeLenses ''Heap
makeLenses ''FunctionValue
makeLenses ''StackFrame
makeLenses ''Stack
makeLenses ''Scope
makeLenses ''ScopeGroup

-- | Run a signaled execution.
runSignaledExecution :: SignaledExecution a -> ProgramCtx -> IO (Either Signal a, ProgramCtx)
runSignaledExecution = runStateT . runExceptT

-- | Utility function to call
unsignaled :: SignaledExecution a -> Execution a
unsignaled = (fromRight (internalPanicPure "unexpected signal") <$>) . runExceptT

-- | Generalised panic function that can be used without specifically returning a
-- | 'CopyValue'. This function is handy when interacting in constructs that
-- | expect the return type to be something other than 'CopyValue'.
panic :: String -> Execution a
panic msg = do
  _ <- liftIO (putStrLn $ "Panic: " ++ msg)
  -- traverse up the stack and print the fnName, and call site
  getBacktrace >>= \t -> liftIO $ putStrLn (T.unpack t)
  liftIO exitFailure

-- | Internal compiler panic function (with backtrace).
internalPanic :: String -> Execution a
internalPanic msg = internalPanic' msg $ getBacktrace >>= \t -> liftIO $ putStrLn $ "Hash crashed at:\n" ++ T.unpack t

-- | Internal compiler panic function for unreachable code (pure).
unreachable :: Execution a
unreachable = internalPanic "reached unreachable case"

-- | Backtrace intrinsic function, take a stack and unwind it to form a readable stack trace.
-- | This can be called at any time, including within the runtime to get the current executiom
-- | task. To get more information on how backtraces are formatted, checkout out this issue on
-- | GH: https://github.com/feds01/hash/issues/81
getBacktrace :: Execution T.Text
getBacktrace = (gets . view $ stack) >>= \(Stack x xs) -> return $ trace (x : xs)
  where
    sites st = group $ map (\x -> (view fnName x, view callSite x)) st
    trace st =
      T.intercalate (T.pack "\n") $
        map
          ( \site -> case length site of
              0 -> T.pack "\tat <unkown>" -- should never happen, but nevertheless.
              1 -> let (f, l) = head site in T.pack ("\tat " ++ f ++ "(" ++ l ++ ")")
              n -> let (f, l) = head site in T.pack ("\tat " ++ f ++ "(" ++ l ++ ")\n" ++ "\t... (" ++ show (n - 1) ++ " similar)") -- group similar messages
          )
          (sites st)
