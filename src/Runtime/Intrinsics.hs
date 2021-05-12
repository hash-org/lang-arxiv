{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- | Hash runtime intrinsics. Functions that cannot be defined within the
-- | program, and must be natively called. This could include IO, Math,
-- | Compiler internals, etc.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Runtime.Intrinsics where

import Control.Exception (AsyncException (HeapOverflow), handle, try)
import Control.Monad (when)
import Control.Monad.Except (MonadTrans (lift), filterM)
import Control.Monad.ListM (allM, findM)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Bits
  ( Bits (complement, shiftL, shiftR, xor, (.&.), (.|.)),
  )
import Data.Char (chr, ord)
import Data.Dynamic (Dynamic, dynTypeRep, fromDynamic, toDyn)
import Data.Fixed (mod')
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (hash)
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import GHC.IO.Exception (IOErrorType (..))
import Runtime.Boot
import Runtime.Heap (heapDeref, heapModify, newHeapVal)
import Runtime.Primitives
import Runtime.VM
  ( evalInstruction,
    heapAllocString,
  )
import qualified System.IO as SIO
import System.IO.Error (ioeGetErrorString, ioeGetErrorType)
import Text.Read (readMaybe)

-- | Argument for invalid types within intrinsic functions
invalidTypes :: Execution a
invalidTypes = panic "Invalid intrinsic types"

-- ############################################################
--                        Compiler Intrinsics
-- ############################################################

-- | Intrinsic function to get the back trace of the current execution context.
intrinsicBacktrace :: IntrinsicFunc
intrinsicBacktrace _ = getBacktrace >>= \bt -> RefValue <$> newHeapVal (StrV bt)

-- | Panicking function, used to stop the runtime in it's tracks.
intrinsicPanic :: IntrinsicFunc
intrinsicPanic [RefValue r] =
  heapDeref r >>= \case
    StrV t -> panic (T.unpack t)
    _ -> invalidTypes
intrinsicPanic _ = invalidTypes -- okay now this is bad (panic within panic)!

intrinsicUnreachable :: IntrinsicFunc
intrinsicUnreachable [] = panic "Reached an unreachable case."
intrinsicUnreachable _ = invalidTypes

-- ############################################################
--                        Intrinsic IO
-- ############################################################

-- | Internal function to Convert an IO Error into our error codes.
mapIOErrorType :: IOErrorType -> Integer
mapIOErrorType AlreadyExists = 2
mapIOErrorType NoSuchThing = 0
mapIOErrorType ResourceBusy = 3
mapIOErrorType ResourceExhausted = 4
mapIOErrorType EOF = 5
mapIOErrorType IllegalOperation = 6
mapIOErrorType _ = -1

-- | Utility function within IO intrinsics to create a struct representing
-- | the 'IoError' struct which consists of an 'error_type' and a 'message'
-- | which is just a string.
convIOError :: IOError -> Execution CopyValue
convIOError e = do
  msg <- heapAllocString (T.pack $ ioeGetErrorString e)
  let typ = IntV (mapIOErrorType $ ioeGetErrorType e)

  -- @Safety: We assume that the IoError struct is packed by having the first fields
  -- as always being the 'error_type' and the second field as being 'message'.
  -- If for some reason this is changed, this will cause some seriously dangerous
  -- behaviour in the 'hash' world.
  unsignaled $ evalInstruction (mkIoErrReturnTup typ msg)

-- | Convert a 'Dynamic' typed object into a 'Handle'. If the conversion fails,
-- | the function will invoke a panic if the conversion fails.
dynToHandle :: Dynamic -> Execution SIO.Handle
dynToHandle h = maybe (internalPanic $ "Expected handle from dynamic object. Got " ++ show (dynTypeRep h)) return (fromDynamic h)

-- | Printing intrinsinc, takes some value (likely on the heap) and prints it
-- | the value to stdout.
intrinsicPrint :: IntrinsicFunc
intrinsicPrint [RefValue r] =
  heapDeref r >>= \case
    StrV t -> (liftIO . TI.putStrLn $ t) >> return VoidV
    _ -> invalidTypes
intrinsicPrint _ = invalidTypes

-- | Intrinsic input, takes some input from stdin and passes it to runtime
intrinsicInput :: IntrinsicFunc
intrinsicInput [] = liftIO (TI.hGetLine SIO.stdin) >>= \s -> RefValue <$> newHeapVal (StrV s)
intrinsicInput _ = invalidTypes

-- | Intrinsic function to put a character into the standard input
intrinsicCharSet :: IntrinsicFunc
intrinsicCharSet [CharV c] = liftIO (SIO.hPutChar SIO.stdin c) >> return VoidV
intrinsicCharSet _ = invalidTypes

-- | Intrinsic function to get a character from standard output
intrinsicCharGet :: IntrinsicFunc
intrinsicCharGet [] = liftIO getChar >>= \x -> return $ CharV x
intrinsicCharGet _ = invalidTypes

-- | Intrinsic function to get a handle to standard output
intrinsicGetStdout :: IntrinsicFunc
intrinsicGetStdout [] = RefValue <$> newHeapVal (NativeV $ toDyn SIO.stdout)
intrinsicGetStdout _ = invalidTypes

-- | Intrinsic function to get a handle to standard input
intrinsicGetStdin :: IntrinsicFunc
intrinsicGetStdin [] = RefValue <$> newHeapVal (NativeV $ toDyn SIO.stdin)
intrinsicGetStdin _ = invalidTypes

-- | Intrinsic function to get a handle to standard error
intrinsicGetStderr :: IntrinsicFunc
intrinsicGetStderr [] = RefValue <$> newHeapVal (NativeV $ toDyn SIO.stderr)
intrinsicGetStderr _ = invalidTypes

-- | Intrinsic function to get a 'Handle' for a specific file with
-- | a specific opening mode. The 'Handle' will be converted into
-- | a 'Dynamic' value, which will be returned as a reference. The
-- | handle is equivalent to the `Native` annotation in the stdlib.
intrinsicOpen :: IntrinsicFunc
intrinsicOpen [RefValue f, IntV m] = do
  heapDeref f >>= \case
    (StrV filename) -> do
      let mode = toEnum (fromInteger m) :: SIO.IOMode

      op <- liftIO $ try $ SIO.openFile (T.unpack filename) mode

      case (op :: Either IOError SIO.Handle) of
        Left e -> convIOError e >>= \x -> resultErrV x
        Right handle -> newHeapVal (NativeV $ toDyn handle) >>= \x -> resultOkV (RefValue x)
    _ -> invalidTypes
intrinsicOpen _ = invalidTypes

-- | Intrinsic function to close a file 'Handle' which is passed as a
-- | reference to a 'Dynamic' object in the heap.
intrinsicClose :: IntrinsicFunc
intrinsicClose [RefValue ref] = do
  heapDeref ref >>= \case
    (NativeV handleDyn) -> do
      h <- dynToHandle handleDyn
      op <- liftIO $ try $ SIO.hClose h

      case (op :: Either IOError ()) of
        Left e -> convIOError e >>= \x -> resultErrV x
        Right _ -> resultOkV VoidV
    _ -> invalidTypes
intrinsicClose _ = invalidTypes

-- | Intrinsic function to put a character from a given file 'Handle'
intrinsicFset :: IntrinsicFunc
intrinsicFset [RefValue ref, CharV ch] = do
  heapDeref ref >>= \case
    (NativeV handleDyn) -> do
      h <- dynToHandle handleDyn
      op <- liftIO $ try $ SIO.hPutChar h ch

      case (op :: Either IOError ()) of
        Left e -> convIOError e >>= \x -> resultErrV x
        Right _ -> resultOkV VoidV
    _ -> invalidTypes
intrinsicFset _ = invalidTypes

-- | Intrinsic function to get a character from a given file 'Handle'
intrinsicFget :: IntrinsicFunc
intrinsicFget [RefValue ref] = do
  heapDeref ref >>= \case
    (NativeV handleDyn) -> do
      h <- dynToHandle handleDyn
      op <- liftIO $ try $ SIO.hGetChar h

      case (op :: Either IOError Char) of
        Left e -> convIOError e >>= \x -> resultErrV x
        Right ch -> resultOkV (CharV ch)
    _ -> invalidTypes
intrinsicFget _ = invalidTypes

-- | Intrinsic function to put a line (\n terminated) into a given file 'Handle'
intrinsicFPrint :: IntrinsicFunc
intrinsicFPrint [RefValue ref, RefValue s] = do
  heapDeref ref >>= \case
    (NativeV handleDyn) -> do
      h <- dynToHandle handleDyn

      -- expecting this to be a string, get it from the heap
      heapDeref s >>= \case
        (StrV str) -> do
          op <- liftIO $ try $ SIO.hPutStrLn h (T.unpack str)

          case (op :: Either IOError ()) of
            Left e -> convIOError e >>= \x -> resultErrV x
            Right _ -> resultOkV VoidV
        _ -> invalidTypes
    _ -> invalidTypes
intrinsicFPrint _ = invalidTypes

-- | Intrinsic function to take a line (\n terminated) into a given file 'Handle'
intrinsicFInput :: IntrinsicFunc
intrinsicFInput [RefValue ref] = do
  heapDeref ref >>= \case
    (NativeV handleDyn) -> do
      h <- dynToHandle handleDyn
      op <- liftIO $ try $ SIO.hGetLine h

      case (op :: Either IOError String) of
        Left e -> convIOError e >>= \x -> resultErrV x
        Right s -> heapAllocString (T.pack s) >>= \x -> resultOkV x
    _ -> invalidTypes
intrinsicFInput _ = invalidTypes

-- | Intrinsic function to modify the ofset from where the handle is
-- | being read, specified by the `SeekMode` enum within the Hash
-- | standard lib. The seek mode can be changed to start reding from
-- | the start, end or with a specified offset.
intrinsicFSeek :: IntrinsicFunc
intrinsicFSeek [RefValue ref, IntV p, IntV w] = do
  heapDeref ref >>= \case
    (NativeV handleDyn) -> do
      h <- dynToHandle handleDyn

      -- convert the 'whence' into a seek mode
      let mode = toEnum (fromInteger w) :: SIO.SeekMode

      op <- liftIO $ try $ SIO.hSeek h mode p

      case (op :: Either IOError ()) of
        Left e -> convIOError e >>= \x -> resultErrV x
        Right _ -> resultOkV VoidV
    _ -> invalidTypes
intrinsicFSeek _ = invalidTypes

-- ############################################################
--            Intrinsics for Bracket type interactions
-- ############################################################

-- | List indexing intrincsic. Takes a reference to a list and a
-- | 'Nat' number index, attempt to index the 'Vector' at the given
-- | index. If the index is out of bounds, the function will call
-- | the 'panic' function since indexing a list out of bounds would
-- | cause undefined behaviour. To use a more `monadic` approach to
-- | indexing, you should use try_index() which returns a result.
intrinsicIndexGet :: IntrinsicFunc
intrinsicIndexGet [RefValue l, IntV k] = do
  heapDeref l >>= \case
    (ListV list) -> maybe reportIndexError return (list S.!? fromInteger k)
      where
        reportIndexError = do
          panic $ "List index out of bounds: " ++ show k
    (StrV string) -> do
      let idx = fromInteger k :: Int
      let len = T.length string
      when (idx > len - 1 || idx < 0) $ do
        panic $ "String index is out of bounds, string length is '" ++ show len ++ "' whereas the index is " ++ show idx

      return $ CharV (T.head (T.drop idx string))
    _ -> invalidTypes
intrinsicIndexGet _ = invalidTypes

-- | Intrinsic function to get the keys from either a set or a map.
intrinsicGetKeys :: IntrinsicFunc
intrinsicGetKeys [RefValue obj] = do
  heapDeref obj >>= \case
    (MapV m) -> do
      let keys = map fst (concat (HM.elems m))
      RefValue <$> newHeapVal (ListV $ S.fromList keys)
    (SetV set) -> do
      let keys = concat $ HM.elems set
      RefValue <$> newHeapVal (ListV $ S.fromList keys)
    _ -> invalidTypes
intrinsicGetKeys _ = invalidTypes

-- | Intrinsic function to get a value from a given map by provided a key,
-- | we require a hashing function that can be used to hash the type of
-- | the key. We allow this since maps can be made of any type which implements
-- | the 'hash' trait, and therefore we allow for someone to 'implement' hashing
-- | for a given type and then that can be used within the map/set implementation.
intrinsicIndexMapGet :: IntrinsicFunc
intrinsicIndexMapGet [RefValue m, key, RefValue hf, RefValue eq] = do
  heapDeref m >>= \case
    (MapV map) -> do
      -- Hash the key
      hashK <- evalHashFn (RefValue hf) key
      items <- maybe (panic "Key not in map") return $ hashK `HM.lookup` map

      x <- findM (\(x, _) -> evalEqFn (RefValue eq) key x) items :: Execution (Maybe (CopyValue, CopyValue))

      case x of
        Just (_, v) -> return v
        Nothing -> panic "Key not in map"
    _ -> invalidTypes
intrinsicIndexMapGet _ = invalidTypes

-- | List mutation intrincsic. Takes a reference to a list and a
-- | 'Nat' number index, and a value that represents the type of the
-- | list that will be inserted into the list. Attempt to index the
-- | 'Vector' at the given  index. If the index is out of bounds, the
-- | function will call the 'panic' function since indexing a list
-- | out of bounds would cause undefined behaviour. To use a more `monadic`
-- | approach to indexing, you should use try_mut_index() which returns
-- | a result. If the index is within bounds, it will update the value in the
-- | vector and just modify the heap reference.
intrinsicIndexMut :: IntrinsicFunc
intrinsicIndexMut [RefValue l, IntV idx, item] = do
  heapModify
    l
    ( \case
        (ListV list) -> do
          -- ensure the list is not being indexxed out of bounds
          if idx < 0 || fromInteger idx >= S.length list
            then do
              panic $ "List index out of bounds: " ++ show idx
            else return . ListV $ S.update (fromInteger idx) item list
        _ -> invalidTypes
    )
    >> return VoidV
intrinsicIndexMut _ = invalidTypes

-- | Intrinsic function to modify a given map with a provided key, value
-- | and a hashing function that can hash the type of the key. We allow
-- | this since maps can be made of any type which implements the 'hash'
-- | trait, and therefore we allow for someone to 'implement' hashing
-- | for a given type and then that can be used within the map/set implementation.
intrinsicIndexMapMut :: IntrinsicFunc
intrinsicIndexMapMut [RefValue m, key, item, RefValue hf, RefValue eq] = do
  heapModify
    m
    ( \case
        (MapV m) -> do
          -- Hash the key
          hashK <- evalHashFn (RefValue hf) key
          MapV
            <$> HM.alterF
              ( \case
                  Just i ->
                    Just
                      <$> mapM
                        ( \(k, v) -> do
                            eq <- evalEqFn (RefValue eq) key k
                            if eq
                              then return (k, v)
                              else return (k, item)
                        )
                        i
                  _ -> return Nothing
              )
              hashK
              m
        _ -> invalidTypes
    )
    >> return VoidV
intrinsicIndexMapMut _ = invalidTypes

intrinsicContains :: IntrinsicFunc
intrinsicContains [RefValue r, q, RefValue eq] = do
  heapDeref r >>= \case
    (ListV l) -> do
      -- use the provided equality function to try and find the item
      x <- findM (evalEqFn (RefValue eq) q) (toList l)

      case x of
        Just _ -> return boolTrueV
        Nothing -> return boolFalseV
    _ -> invalidTypes
intrinsicContains _ = invalidTypes

-- | Intrinsic function to check whether an element is within the  bracketted type,
-- | with the provided hashing function, equality function. Since we work with buckets,
-- | we have to work by checking that the provided value is within the bucket.
intrinsicBrackettedContains :: IntrinsicFunc
intrinsicBrackettedContains [RefValue r, key, RefValue hf, RefValue eq] = do
  heapDeref r >>= \case
    (MapV m) -> do
      -- Hash the key
      hashK <- evalHashFn (RefValue hf) key
      let items = hashK `HM.lookup` m
      case items of
        Just i -> do
          x <- findM (\(x, _) -> evalEqFn (RefValue eq) key x) i :: Execution (Maybe (CopyValue, CopyValue))
          case x of
            Just _ -> return boolTrueV
            _ -> return boolFalseV
        _ -> return boolFalseV
    (SetV s) -> do
      -- Hash the key
      hashK <- evalHashFn (RefValue hf) key
      let items = hashK `HM.lookup` s
      case items of
        Just i -> do
          x <- findM (evalEqFn (RefValue eq) key) i :: Execution (Maybe CopyValue)
          case x of
            Just _ -> return boolTrueV
            _ -> return boolFalseV
        _ -> return boolFalseV
    _ -> invalidTypes
intrinsicBrackettedContains _ = invalidTypes

-- | Intrinsic to add an element to the start of the list.
intrinsicListPush :: IntrinsicFunc
intrinsicListPush [RefValue r, a] = do
  -- append an element to a list
  heapModify
    r
    ( \case
        (ListV l) -> return $ ListV (l S.|> a)
        _ -> invalidTypes
    )
    >> return VoidV
intrinsicListPush _ = invalidTypes

-- | Intrinsic to add an element to the end of the list.
intrinsicListPushFront :: IntrinsicFunc
intrinsicListPushFront [RefValue r, a] = do
  -- append an element to a list
  heapModify
    r
    ( \case
        (ListV l) -> return $ ListV (a S.<| l)
        _ -> invalidTypes
    )
    >> return VoidV
intrinsicListPushFront _ = invalidTypes

-- | Intrinsic to add an element to the end of the list.
intrinsicListPop :: IntrinsicFunc
intrinsicListPop [RefValue r] = do
  -- append an element to a list
  heapModify
    r
    ( \case
        (ListV l) -> case length l of
          0 -> panic "Cannot pop a list with zero elements"
          len -> return $ ListV (S.take (len - 1) l)
        _ -> invalidTypes
    )
    >> return VoidV
intrinsicListPop _ = invalidTypes

-- | Intrinsic for inserting an element at a given index within a list
intrinsicListInsert :: IntrinsicFunc
intrinsicListInsert [RefValue l, IntV idx, item] = do
  heapModify
    l
    ( \case
        (ListV list) -> do
          let safeIdx = fromInteger idx

          if idx < 0 || safeIdx >= S.length list
            then do
              panic $ "List index out of bounds: " ++ show idx
            else return $ ListV (S.insertAt safeIdx item list)
        _ -> invalidTypes
    )
    >> return VoidV
intrinsicListInsert _ = invalidTypes

-- | Intrinsic for removal of an element from a list at a given index
intrinsicListRemove :: IntrinsicFunc
intrinsicListRemove [RefValue l, IntV idx] = do
  heapModify
    l
    ( \case
        (ListV list) -> do
          let safeIdx = fromInteger idx
          if idx < 0 || fromInteger idx >= S.length list
            then do
              panic $ "List index out of bounds: " ++ show idx
            else return $ ListV (S.deleteAt safeIdx list)
        _ -> invalidTypes
    )
    >> return VoidV
intrinsicListRemove _ = invalidTypes

-- | Intrinsic for generic removal of an element from a either a
-- | set or a map.
intrinsicBrackettedRemove :: IntrinsicFunc
intrinsicBrackettedRemove [RefValue r, key, RefValue hf, RefValue eq] =
  do
    heapModify
  r
  ( \case
      (MapV m) -> do
        -- Hash the key
        hashK <- evalHashFn (RefValue hf) key
        MapV
          <$> HM.alterF
            ( \case
                Just i -> Just <$> filterM (\(x, _) -> not <$> evalEqFn (RefValue eq) key x) i
                _ -> return Nothing
            )
            hashK
            m
      (SetV s) -> do
        -- Hash the key
        hashK <- evalHashFn (RefValue hf) key
        SetV
          <$> HM.alterF
            ( \case
                Just i -> Just <$> filterM ((not <$>) . evalEqFn (RefValue eq) key) i
                _ -> return Nothing
            )
            hashK
            s
      _ -> invalidTypes
  )
    >> return VoidV
intrinsicBrackettedRemove _ = invalidTypes

-- ############################################################
--                      Intrinsic Operators
-- ############################################################

-- | Equality intrinsic, implements equality for primitive types such
-- | as Int, Char, Float, and string. This intrinsic does not support
-- | any kind of bracketted type due to other types require a reference
-- | to a comparison function in order to compare the members
intrinsicEq :: IntrinsicFunc
intrinsicEq [IntV l, IntV r] = return $ boolV (l == r)
intrinsicEq [CharV l, CharV r] = return $ boolV (l == r)
intrinsicEq [FloatV l, FloatV r] = return $ boolV (l == r)
intrinsicEq [RefValue leftR, RefValue rightR] = do
  lRef <- heapDeref leftR
  rRef <- heapDeref rightR

  case (lRef, rRef) of
    (StrV l, StrV r) -> return $ boolV (l == r)
    _ -> invalidTypes
intrinsicEq _ = invalidTypes

-- | Convert an Ordering to an integer copy value.
haskellOrdToInt :: Ordering -> CopyValue
haskellOrdToInt =
  IntV . \case
    EQ -> 0
    LT -> 1
    GT -> -1

-- | Equality intrinsic, implements equality for primitive types such
-- | as Int, Char, Float, and string. This intrinsic does not support
-- | any kind of bracketted type due to other types require a reference
-- | to a comparison function in order to compare the members
intrinsicOrd :: [CopyValue] -> Execution CopyValue
intrinsicOrd [IntV l, IntV r] = return (haskellOrdToInt $ compare l r)
intrinsicOrd [CharV l, CharV r] = return (haskellOrdToInt $ compare l r)
intrinsicOrd [FloatV l, FloatV r] = return (haskellOrdToInt $ compare l r)
intrinsicOrd [RefValue leftR, RefValue rightR] = do
  lRef <- heapDeref leftR
  rRef <- heapDeref rightR

  case (lRef, rRef) of
    (StrV l, StrV r) -> return (haskellOrdToInt $ compare l r)
    _ -> invalidTypes
intrinsicOrd _ = invalidTypes

-- | Intrinsic map equality function. This funciton is used to compare a
-- | map primitive by taking to maps, and two references to comparison functions
-- | that can be used to compare the type of the `key` and the type of the `value`.
-- | This function implements short circuiting behaviour, if a comparison fails
-- | before evalutating the whole of the map, it will exit early.
intrinsicEqMap :: IntrinsicFunc
intrinsicEqMap [RefValue leftR, RefValue rightR, RefValue compA, RefValue compB] = do
  lRef <- heapDeref leftR
  rRef <- heapDeref rightR

  -- @@ FIXME!!!

  case (lRef, rRef) of
    (MapV l, MapV r) -> do
      if HM.size l /= HM.size r
        then return boolFalseV
        else do
          entries <-
            allM
              ( \((_, kvs1), (_, kvs2)) -> do
                  allM
                    ( \((k1, v1), (k2, v2)) -> do
                        keyComp <- evalEqFn (RefValue compA) k1 k2
                        valComp <- evalEqFn (RefValue compB) v1 v2

                        return $ valComp && keyComp
                    )
                    (zip kvs1 kvs2)
              )
              (zip (HM.toList l) (HM.toList r))

          return $ boolV entries
    _ -> invalidTypes
intrinsicEqMap _ = invalidTypes

-- | Intrinsic list/set equality function. This funciton is used to compare a
-- | bracketted primitive by taking two bracketted types, and a references to
-- | a comparison function that can be used to compare the type of the `key`.
-- | This function implements short circuiting behaviour, if a comparison fails
-- | before evalutating the all of the elements of the type, it will exit early.
intrinsicEqBracketted :: IntrinsicFunc
intrinsicEqBracketted [RefValue leftR, RefValue rightR, RefValue compA] = do
  lRef <- heapDeref leftR
  rRef <- heapDeref rightR

  -- @@ FIXME!!!

  case (lRef, rRef) of
    (ListV l, ListV r) -> do
      if S.length l /= S.length r
        then return boolFalseV
        else do
          entries <- allM (uncurry (evalEqFn (RefValue compA))) (S.zip l r)

          return $ boolV entries
    (SetV l, SetV r) -> do
      if HM.size l /= HM.size r
        then return boolFalseV
        else do
          entries <-
            allM
              ( \((_, vs1), (_, vs2)) -> do
                  allM (uncurry $ evalEqFn (RefValue compA)) (zip vs1 vs2)
              )
              (zip (HM.toList l) (HM.toList r))

          return $ boolV entries
    _ -> invalidTypes
intrinsicEqBracketted _ = invalidTypes

-- | Intrinsic for reference equality. Simply compares two references
-- | within the 'Heap' of the current running context.
intrinsicRefEq :: IntrinsicFunc
intrinsicRefEq [RefValue l, RefValue r] = return $ boolV (l == r)
intrinsicRefEq _ = invalidTypes

-- | Add operator intrinsic, supported for Int, Float, String and
-- | all the mutable bracket types, which are List, Map, Set. Since
-- | this intrinsic adds the constructs together, it does not require
-- | a hashing function unlike when adding an element to either of
-- | one of those types.
intrinsicAdd :: IntrinsicFunc
intrinsicAdd [IntV l, IntV r] = return $ IntV (l + r)
intrinsicAdd [FloatV l, FloatV r] = return $ FloatV (l + r)
intrinsicAdd [RefValue leftR, RefValue rightR] = do
  lRef <- heapDeref leftR
  rRef <- heapDeref rightR

  case (lRef, rRef) of
    (ListV l, ListV r) -> RefValue <$> newHeapVal (ListV $ l S.>< r)
    (SetV l, SetV r) -> RefValue <$> newHeapVal (SetV $ l `HM.union` r)
    (MapV l, MapV r) -> RefValue <$> newHeapVal (MapV $ l `HM.union` r)
    (StrV l, StrV r) -> RefValue <$> newHeapVal (StrV $ l `T.append` r)
    _ -> invalidTypes
intrinsicAdd _ = invalidTypes

-- | Subtract operator intrinsic, supported for Int and Float
intrinsicSub :: IntrinsicFunc
intrinsicSub [IntV l, IntV r] = return $ IntV (l - r)
intrinsicSub [FloatV l, FloatV r] = return $ FloatV (l - r)
intrinsicSub _ = invalidTypes

-- | Multiply operator intrinsic, supported for Int and Float
intrinsicMul :: IntrinsicFunc
intrinsicMul [IntV l, IntV r] = return $ IntV (l * r)
intrinsicMul [FloatV l, FloatV r] = return $ FloatV (l * r)
intrinsicMul _ = invalidTypes

-- | Divide operator intrinsic, supported for Int and Float
intrinsicDiv :: IntrinsicFunc
intrinsicDiv [IntV l, IntV r] = return $ IntV (l `div` r)
intrinsicDiv [FloatV l, FloatV r] = return $ FloatV (l / r)
intrinsicDiv _ = invalidTypes

-- | Negation (unary) operator intrinsic, supported for Int and Float
intrinsicNeg :: IntrinsicFunc
intrinsicNeg [IntV l] = return $ IntV (- l)
intrinsicNeg [FloatV l] = return $ FloatV (- l)
intrinsicNeg _ = invalidTypes

-- | Modulus operator intrinsic, supported for Int and Float
intrinsicMod :: IntrinsicFunc
intrinsicMod [IntV l, IntV r] = return $ IntV (l `mod` r)
intrinsicMod [FloatV l, FloatV r] = return $ FloatV (l `mod'` r)
intrinsicMod _ = invalidTypes

-- ############################################################
--                 Intrinsic Bitwise Operations
-- ############################################################

-- | Intrinsic right bit shift function
intrinsicBitShr :: IntrinsicFunc
intrinsicBitShr [IntV l, IntV r] = do
  item <-
    liftIO $
      handle (\HeapOverflow -> return Nothing) $
        return . Just $ IntV (l `shiftR` fromIntegral r)

  -- If right handside is greater than the 'Int' type size, it is bound by int
  maybe (internalPanic "Integer overflow!") return item
intrinsicBitShr _ = invalidTypes

-- | Intrinsic left bit shift function
intrinsicBitShl :: IntrinsicFunc
intrinsicBitShl [IntV l, IntV r] = do
  item <-
    liftIO $
      handle (\HeapOverflow -> return Nothing) $
        return . Just $ IntV (l `shiftL` fromIntegral r)

  -- If right handside is greater than the 'Int' type size, it is bound by int
  maybe (internalPanic "Integer overflow!") return item
intrinsicBitShl _ = invalidTypes

-- | Intrinsic bitwise not function
intrinsicBitNot :: IntrinsicFunc
intrinsicBitNot [IntV l] = return $ IntV (complement l)
intrinsicBitNot _ = invalidTypes

-- | Intrinsic biwise and function
intrinsicBitAnd :: IntrinsicFunc
intrinsicBitAnd [IntV l, IntV r] = return $ IntV (l .&. r)
intrinsicBitAnd _ = invalidTypes

-- | Intrinsic biwise or function
intrinsicBitOr :: IntrinsicFunc
intrinsicBitOr [IntV l, IntV r] = return $ IntV (l .|. r)
intrinsicBitOr _ = invalidTypes

-- | Intrinsic biwise xor function
intrinsicBitXor :: IntrinsicFunc
intrinsicBitXor [IntV l, IntV r] = return $ IntV (l `xor` r)
intrinsicBitXor _ = invalidTypes

-- ############################################################
--                        Intrinsic Math
-- ############################################################

-- | Intrinsic floor function
intrinsicFloor :: IntrinsicFunc
intrinsicFloor [FloatV v] = return $ FloatV (fromInteger $ floor v)
intrinsicFloor _ = invalidTypes

-- | Intrinsic ceil function
intrinsicCeil :: IntrinsicFunc
intrinsicCeil [FloatV v] = return $ FloatV (fromInteger $ ceiling v)
intrinsicCeil _ = invalidTypes

-- | Intrinsic round function
intrinsicRound :: IntrinsicFunc
intrinsicRound [FloatV v] = return $ FloatV (fromInteger $ round v)
intrinsicRound _ = invalidTypes

-- | Intrinsic sin function
intrinsicSin :: IntrinsicFunc
intrinsicSin [FloatV v] = return $ FloatV (sin v)
intrinsicSin _ = invalidTypes

-- | Intrinsic cos function
intrinsicCos :: IntrinsicFunc
intrinsicCos [FloatV v] = return $ FloatV (cos v)
intrinsicCos _ = invalidTypes

-- | Intrinsic tan function
intrinsicTan :: IntrinsicFunc
intrinsicTan [FloatV v] = return $ FloatV (tan v)
intrinsicTan _ = invalidTypes

-- | Intrinsic arcsin function
intrinsicAsin :: IntrinsicFunc
intrinsicAsin [FloatV v] = return $ FloatV (asin v)
intrinsicAsin _ = invalidTypes

-- | Intrinsic arccos function
intrinsicAcos :: IntrinsicFunc
intrinsicAcos [FloatV v] = return $ FloatV (acos v)
intrinsicAcos _ = invalidTypes

-- | Intrinsic arctan function
intrinsicAtan :: IntrinsicFunc
intrinsicAtan [FloatV v] = return $ FloatV (atan v)
intrinsicAtan _ = invalidTypes

-- | Intrinsic hyperbolic sin function
intrinsicSinh :: IntrinsicFunc
intrinsicSinh [FloatV v] = return $ FloatV (sinh v)
intrinsicSinh _ = invalidTypes

-- | Intrinsic hyperbolic cos function
intrinsicCosh :: IntrinsicFunc
intrinsicCosh [FloatV v] = return $ FloatV (cosh v)
intrinsicCosh _ = invalidTypes

-- | Intrinsic hyperbolic tan function
intrinsicTanh :: IntrinsicFunc
intrinsicTanh [FloatV v] = return $ FloatV (tanh v)
intrinsicTanh _ = invalidTypes

-- | Intrinsic hyperbolic arcsin function
intrinsicAsinh :: IntrinsicFunc
intrinsicAsinh [FloatV v] = return $ FloatV (asinh v)
intrinsicAsinh _ = invalidTypes

-- | Intrinsic hyperbolic arccos function
intrinsicAcosh :: IntrinsicFunc
intrinsicAcosh [FloatV v] = return $ FloatV (acosh v)
intrinsicAcosh _ = invalidTypes

-- | Intrinsic hyperbolic arctan function
intrinsicAtanh :: IntrinsicFunc
intrinsicAtanh [FloatV v] = return $ FloatV (atanh v)
intrinsicAtanh _ = invalidTypes

-- | Intrinsic logarithm function, taking a value and a base which
-- | supports float and integer inputs.
intrinsicLog :: IntrinsicFunc -- log(value, base)
intrinsicLog [FloatV value, FloatV base] = return $ FloatV (base `logBase` value)
intrinsicLog _ = invalidTypes

-- | Intrinsic square root function, taking a value which
-- | supports float and integer inputs.
intrinsicSqrt :: IntrinsicFunc
intrinsicSqrt [FloatV v] = return $ FloatV (sqrt v)
intrinsicSqrt _ = invalidTypes

-- | Intrinsic pow() function, taking a value and an exponent which
-- | supports float and integer inputs.
intrinsicPow :: IntrinsicFunc
intrinsicPow [IntV a, IntV b] = return $ IntV (a ^ b)
intrinsicPow [FloatV a, FloatV b] = return $ FloatV (a ** b)
intrinsicPow _ = invalidTypes

-- ############################################################
--                        General Intrinsics
-- ############################################################

-- | Get a slice of a string
intrinsicSlice :: IntrinsicFunc
intrinsicSlice [RefValue r, IntV begin, IntV end] = do
  heapDeref r >>= \case
    (StrV value) -> do
      let b = fromInteger begin :: Int
      let e = fromInteger end :: Int

      when (e > b) $ panic "String slice start is larger than end"
      when (e < 0 || b > 0) $ panic "String slices cannot be negative"

      when (b > T.length value) $ do
        panic "String slice start parameter out of bounds"

      let (_, rest) = T.splitAt b value

      -- check if it's still valid to take the rest here
      when ((e - b) > T.length rest) $ do
        panic "String slice end parameter out of bounds"

      heapAllocString (T.take (e - b) rest) >>= \x -> return x
    _ -> invalidTypes
intrinsicSlice _ = invalidTypes

-- | Function to evaluate a given conversion(to_str) function for two parameters
evalConvFn :: CopyValue -> CopyValue -> Execution T.Text
evalConvFn (RefValue r) item = do
  unsignaled $
    evalInstruction (IFnCall "<unkown>" (IRef r) [ILiteral item]) >>= \case
      (RefValue h) -> do
        lift $
          heapDeref h >>= \case
            (StrV v) -> return v
            _ -> internalPanic "Expected str from RefV in conv"
      _ -> lift $ internalPanic "Expected RefV from conv"
evalConvFn _ _ = invalidTypes

-- | Use a function that is referenced within the runtime to hash a
-- | copy value.
evalHashFn :: CopyValue -> CopyValue -> Execution Hashed
evalHashFn (RefValue r) key = do
  unsignaled $
    evalInstruction (IFnCall "<unkown>" (IRef r) [ILiteral key]) >>= \case
      IntV h -> return $ Hashed h
      _ -> lift $ internalPanic "Expected IntV from hashing"
evalHashFn _ _ = invalidTypes

-- | Use a function that is referenced within the runtime to perform a
-- | comparison on a left and right 'CopyValue'
evalEqFn :: CopyValue -> CopyValue -> CopyValue -> Execution Bool
evalEqFn (RefValue eqFn) l r = do
  unsignaled $
    evalInstruction (IFnCall "<unkown>" (IRef eqFn) [ILiteral l, ILiteral r]) >>= \case
      b | b == boolTrueV -> return True
      b | b == boolFalseV -> return False
      e -> lift $ internalPanic $ "expected BoolV comparison " ++ show e
evalEqFn _ _ _ = invalidTypes

-- | Intrinsic hash function, use the provided 'hash' function to convert
-- | a provided value or reference that can be hashed and then returned as
-- | an 'Int'. This is used internally to support adding keys and values to
-- | map and set constructurs within hash.
intrinsicHash :: IntrinsicFunc
intrinsicHash [IntV i] = return $ IntV (fromIntegral $ hash i)
intrinsicHash [FloatV f] = return $ IntV (fromIntegral $ hash f)
intrinsicHash [CharV c] = return $ IntV (fromIntegral $ hash c)
intrinsicHash [RefValue r] = do
  heapDeref r >>= \case
    (StrV value) -> return $ IntV (fromIntegral $ hash value)
    _ -> invalidTypes
intrinsicHash _ = invalidTypes

-- | Intrinsic to get the size of some bracketted type like a set, map
-- | list or a string. Although, note that string length is \O(n)\
intrinsicSize :: IntrinsicFunc
intrinsicSize [RefValue r] = do
  heapDeref r >>= \case
    (StrV s) -> return $ IntV (fromIntegral $ T.length s)
    (MapV m) -> return $ IntV (fromIntegral $ HM.size m)
    (SetV s) -> return $ IntV (fromIntegral $ HM.size s)
    (ListV l) -> return $ IntV (fromIntegral $ S.length l)
    _ -> invalidTypes
intrinsicSize _ = invalidTypes

-- | Intrinsic function to convert attempt to convert some value into a
-- | character value. This intrinsic function only suppoers two variants,
-- | convert an int into a character, and finally convert a character into
-- | itself.
intrinsicConvToChar :: IntrinsicFunc
intrinsicConvToChar [IntV c] =
  if fromIntegral (ord (maxBound :: Char)) < c
    then do
      resultErrV =<< heapAllocString (T.pack "Integer Literal overflow, cannot cast to char!")
    else do
      -- if you give it a negative value, we should return an erorr here
      if c < 0
        then resultErrV =<< heapAllocString (T.pack "Integer Literal overflow, cannot cast to char!")
        else resultOkV (CharV $ chr $ fromIntegral c)
intrinsicConvToChar [CharV c] = resultOkV (CharV c)
intrinsicConvToChar _ = invalidTypes

-- | Intrinsic function to convert attempt to convert some value into a
-- | integer value. This intrinsic function supports 4 variants,
-- | convert an int into itself, and convert an int into a float, character
-- | or string.
intrinsicConvToInt :: IntrinsicFunc
intrinsicConvToInt [IntV i] = resultOkV $ IntV i
intrinsicConvToInt [FloatV f] = resultOkV (IntV $ round f)
intrinsicConvToInt [CharV c] = resultOkV $ IntV (fromIntegral $ ord c) -- @Safety: we can always convert a character to an int
intrinsicConvToInt [RefValue r] = do
  heapDeref r >>= \case
    (StrV str) -> do
      case (readMaybe (T.unpack str) :: Maybe Integer) of -- @@Cleanup: we should use our own 'float' parser to standardise behaviour
        Just f -> resultOkV $ IntV f
        Nothing -> heapAllocString (T.pack "Couldn't parse int from string.") >>= \x -> resultErrV x
    _ -> invalidTypes
intrinsicConvToInt _ = invalidTypes

-- | Intrinsic function to convert attempt to convert some value into a
-- | string value. This intrinsic function supports 4 variants,
-- | convert an string into itself, and convert a string into an int, float,
-- | or character.
intrinsicConvToStr :: [CopyValue] -> Execution CopyValue
intrinsicConvToStr [IntV i] = heapAllocString (T.pack $ show i)
intrinsicConvToStr [FloatV f] = heapAllocString (T.pack $ show f)
intrinsicConvToStr [CharV c] = heapAllocString (T.singleton c)
intrinsicConvToStr [VoidV] = heapAllocString (T.pack "void")
intrinsicConvToStr [RefValue r] = do
  heapDeref r >>= \case
    (StrV _) -> return $ RefValue r
    (FunctionV func) -> heapAllocString (T.pack $ show func)
    (NativeFnV func) -> heapAllocString (T.pack $ show func)
    -- If no implementation exists, we'll just print the reference in the VM
    _ -> heapAllocString (T.pack $ "<ref " ++ show r ++ ">")
intrinsicConvToStr _ = internalPanic "Invalid"

-- | Intrinsic conversion function used to convert a map data structure into a string.
-- | Provide the function with two functions that can convert the key into a string,
-- | and a value into a string, and the function will build up a string representation
-- | of the map data structure.
intrinsicConvMapToStr :: IntrinsicFunc
intrinsicConvMapToStr [RefValue m, RefValue keyConv, RefValue valConv] = do
  heapDeref m >>= \case
    (MapV map) -> do
      if HM.size map == 0
        then heapAllocString (T.pack "{:}") >>= \x -> resultOkV x
        else do
          elems <-
            mapM
              ( \(Hashed _, [(key, val)]) -> do
                  keyS <- evalConvFn (RefValue keyConv) key
                  valS <- evalConvFn (RefValue valConv) val

                  return $ T.intercalate (T.pack ": ") [keyS, valS]
              )
              (HM.toList map)

          -- run provided convToStr function on each element and then
          heapAllocString
            (T.concat [T.pack "{", T.intercalate (T.pack ", ") elems, T.pack "}"])
            >>= \x -> resultOkV x
    _ -> invalidTypes
intrinsicConvMapToStr _ = invalidTypes

-- | Intrinsic conversion function used to convert a set or list data structure into
-- | a string. Provide a function that can convert the type of the element into a
-- | into a string and the function will build up a string representation of the
-- | bracketted type.
intrinsicConvBrackettedToStr :: IntrinsicFunc
intrinsicConvBrackettedToStr [RefValue b, RefValue keyConv] = do
  heapDeref b >>= \case
    (SetV set) -> do
      if HM.size set == 0
        then heapAllocString (T.pack "{,}") >>= \x -> resultOkV x
        else do
          elems <-
            mapM
              (\(Hashed _, [keys]) -> evalConvFn (RefValue keyConv) keys >>= \x -> return x)
              (HM.toList set)

          -- run provided convToStr function on each element and then
          heapAllocString
            (T.concat [T.pack "{", T.intercalate (T.pack ", ") elems, T.pack "}"])
            >>= \x -> resultOkV x
    (ListV list) -> do
      if S.length list == 0
        then heapAllocString (T.pack "[]") >>= \x -> resultOkV x
        else do
          elems <-
            mapM
              (evalConvFn (RefValue keyConv))
              list
          -- run provided convToStr function on each element and then
          heapAllocString
            (T.concat [T.pack "[", T.intercalate (T.pack ", ") (toList elems), T.pack "]"])
            >>= \x -> resultOkV x
    _ -> invalidTypes
intrinsicConvBrackettedToStr _ = invalidTypes

-- | Intrinsic function to convert attempt to convert some value into a
-- | float value. This intrinsic function supports three variants, convert an int
-- | or a string into a float, and a float into itself.
intrinsicConvToFloat :: IntrinsicFunc
intrinsicConvToFloat [IntV i] = resultOkV $ FloatV (fromIntegral i)
intrinsicConvToFloat [FloatV f] = resultOkV $ FloatV f
intrinsicConvToFloat [RefValue r] = do
  heapDeref r >>= \case
    (StrV str) -> do
      case (readMaybe (T.unpack str) :: Maybe Double) of -- @@Cleanup: we should use our own 'float' parser to standardise behaviour
        Just f -> resultOkV $ FloatV f
        Nothing -> heapAllocString (T.pack "Couldn't parse float from string.") >>= \x -> resultErrV x
    _ -> invalidTypes
intrinsicConvToFloat _ = invalidTypes

-- ############################################################
--                          Export
-- ############################################################

-- | Intrinsic starting heap index.
intrinsicIndexStart :: Int
intrinsicIndexStart = 1

-- | Transform the intrinsic map to an indexed version (for the runtime heap)
intrinsicIndexed :: [(String, a)] -> [(String, (Ref, a))]
intrinsicIndexed = zipWith (curry (\(idx, (k, v)) -> (k, (Ref idx, v)))) [intrinsicIndexStart ..]

-- | Intrinsic Map exports all the defined intrinsics.
intrinsicMap :: HM.HashMap String (Ref, [CopyValue] -> Execution CopyValue)
intrinsicMap =
  HM.fromList $
    intrinsicIndexed
      [ ("intrinsic_panic", intrinsicPanic),
        ("intrinsic_unreachable", intrinsicUnreachable),
        ("intrinsic_backtrace", intrinsicBacktrace),
        ("intrinsic_input", intrinsicInput),
        ("intrinsic_print", intrinsicPrint),
        ("intrinsic_char_get", intrinsicCharGet),
        ("intrinsic_char_set", intrinsicCharSet),
        ("intrinsic_get_stdout", intrinsicGetStdout),
        ("intrinsic_get_stdin", intrinsicGetStdin),
        ("intrinsic_get_stderr", intrinsicGetStderr),
        ("intrinsic_open", intrinsicOpen),
        ("intrinsic_close", intrinsicClose),
        ("intrinsic_fset", intrinsicFset),
        ("intrinsic_fget", intrinsicFget),
        ("intrinsic_fprint", intrinsicFPrint),
        ("intrinsic_finput", intrinsicFInput),
        ("intrinsic_fseek", intrinsicFSeek),
        ("intrinsic_eq", intrinsicEq),
        ("intrinsic_ord", intrinsicOrd),
        ("intrinsic_eq_map", intrinsicEqMap),
        ("intrinsic_eq_bracketted", intrinsicEqBracketted),
        ("intrinsic_ref_eq", intrinsicRefEq),
        ("intrinsic_index_get", intrinsicIndexGet),
        ("intrinsic_index_mut", intrinsicIndexMut),
        ("intrinsic_index_map_get", intrinsicIndexMapGet),
        ("intrinsic_index_map_mut", intrinsicIndexMapMut),
        ("intrinsic_contains", intrinsicContains),
        ("intrinsic_bracketted_contains", intrinsicBrackettedContains),
        ("intrinsic_list_remove", intrinsicListRemove),
        ("intrinsic_bracketted_remove", intrinsicBrackettedRemove),
        ("intrinsic_list_push", intrinsicListPush),
        ("intrinsic_list_pop", intrinsicListPop),
        ("intrinsic_list_push_front", intrinsicListPushFront),
        ("intrinsic_list_insert", intrinsicListInsert),
        ("intrinsic_slice", intrinsicSlice),
        ("intrinsic_size", intrinsicSize),
        ("intrinsic_hash", intrinsicHash),
        ("intrinsic_conv_to_char", intrinsicConvToChar),
        ("intrinsic_conv_to_str", intrinsicConvToStr),
        ("intrinsic_conv_map_to_str", intrinsicConvMapToStr),
        ("intrinsic_conv_bracketted_to_str", intrinsicConvBrackettedToStr),
        ("intrinsic_conv_to_float", intrinsicConvToFloat),
        ("intrinsic_conv_to_int", intrinsicConvToInt),
        ("intrinsic_add", intrinsicAdd),
        ("intrinsic_sub", intrinsicSub),
        ("intrinsic_div", intrinsicDiv),
        ("intrinsic_mul", intrinsicMul),
        ("intrinsic_neg", intrinsicNeg),
        ("intrinsic_mod", intrinsicMod),
        ("intrinsic_bit_shr", intrinsicBitShr),
        ("intrinsic_bit_shl", intrinsicBitShl),
        ("intrinsic_bit_not", intrinsicBitNot),
        ("intrinsic_bit_and", intrinsicBitAnd),
        ("intrinsic_bit_or", intrinsicBitOr),
        ("intrinsic_bit_xor", intrinsicBitXor),
        ("intrinsic_floor", intrinsicFloor),
        ("intrinsic_ceil", intrinsicCeil),
        ("intrinsic_round", intrinsicRound),
        ("intrinsic_sin", intrinsicSin),
        ("intrinsic_cos", intrinsicCos),
        ("intrinsic_tan", intrinsicTan),
        ("intrinsic_asin", intrinsicAsin),
        ("intrinsic_acos", intrinsicAcos),
        ("intrinsic_atan", intrinsicAtan),
        ("intrinsic_sinh", intrinsicSinh),
        ("intrinsic_cosh", intrinsicCosh),
        ("intrinsic_tanh", intrinsicTanh),
        ("intrinsic_asinh", intrinsicAsinh),
        ("intrinsic_acosh", intrinsicAcosh),
        ("intrinsic_atanh", intrinsicAtanh),
        ("intrinsic_log", intrinsicLog),
        ("intrinsic_pow", intrinsicPow),
        ("intrinsic_sqrt", intrinsicSqrt)
      ] -- we trust GHC to not re-evaluate this each time

-- | Utility function to get all of the function names for the intrinsics
-- | defomed within the runtime
intrinsicNames :: [String]
intrinsicNames = HM.keys intrinsicMap
