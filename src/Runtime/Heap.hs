-- | Hash compiler heap utilities and definitions.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Runtime.Heap where

import Control.Lens (over, view, (+~), (.~))
import Control.Monad (when)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Runtime.Boot
import Text.Pretty.Simple (pPrint)

-- | Add a new value to the heap.
newHeapVal :: HeapValue -> Execution Ref
newHeapVal val = do
  -- Add 1 to heap counter.
  modify $ over (heap . heapCounter) (+ 1)
  c <- gets $ view (heap . heapCounter)

  -- Create the new entry
  let vSize = sizeof val
  let newEntry = HeapEntry val vSize

  -- Add it to heap
  modify $ over (heap . heapData) (HM.insert (Ref c) newEntry)

  -- Increment heap size
  modify $ (heap . heapSize) +~ vSize

  -- Return the ref
  return (Ref c)

-- | Get a `HeapValue` given a `Ref`.
--
-- Internal panic if there is no such value.
heapDeref :: Ref -> Execution HeapValue
heapDeref = (view heapVal <$>) . heapDerefEntry

-- | Get a `HeapEntry` given a `Ref`.
--
-- Internal panic if there is no such entry.
heapDerefEntry :: Ref -> Execution HeapEntry
heapDerefEntry r =
  gets (view (heap . heapData)) >>= \h -> case r `HM.lookup` h of
    Just v -> return v
    Nothing -> do
      (liftIO . pPrint) =<< gets (view heap)
      internalPanic $ "Invalid pointer dereference: " ++ show r

-- | Modify a `HeapValue` given a `Ref`, using the provided function.
--
-- Internal panic if there is no such value.
heapModify :: Ref -> (HeapValue -> Execution HeapValue) -> Execution ()
heapModify r f = do
  -- Get old entry
  (HeapEntry oldVal oldSize) <- heapDerefEntry r

  -- Calculate new value
  newVal <- f oldVal

  -- Get new size
  let newSize = sizeof newVal

  -- Set new entry
  let newEntry = HeapEntry newVal newSize
  modify $ over (heap . heapData) (HM.adjust (const newEntry) r)

  -- Set new heap size
  modify $ (heap . heapSize) +~ (newSize - oldSize)

-- | Perform garbage collection on the current heap, keeping given extra
-- references.
garbageCollect :: HS.HashSet Ref -> Execution ()
garbageCollect extraRefs = do
  st <- gets $ view stack

  -- Get the refs that are alive
  let refs = refTrace st `HS.union` extraRefs

  -- Get the heap data filtered by refs, and calculate the new size.
  currHeapData <- gets $ view (heap . heapData)
  let newHeapData =
        HM.filterWithKey
          ( \k v ->
              k `HS.member` refs || case view heapVal v of
                NativeFnV _ -> True -- Don't GC native functions
                NativeV _ -> True -- Don't GC native values
                _ -> False
          )
          currHeapData
  let newHeapSize = HM.foldr (\v acc -> view valSize v + acc) 0 newHeapData

  -- Set new data and size
  modify $ (heap . heapData) .~ newHeapData
  modify $ (heap . heapSize) .~ newHeapSize

  -- Set new info
  setNewHeapInfo

-- | Set new heap size information after garbage collection.
setNewHeapInfo :: Execution ()
setNewHeapInfo = do
  newSize <- gets $ view (heap . heapSize)
  let newMaxSize = round (gcMultiplier * fromIntegral newSize)

  -- Set new max to size after GC times gcMultiplier.
  modify $ (heap . maxSizeBeforeGc) .~ newMaxSize

-- | How much to increase max heap size after a GC cycle, in units of the new
-- heap size.
gcMultiplier :: Float
gcMultiplier = 2

-- | Run a SignaledExecution and potentially run garbage collection afterwards.
gc :: SignaledExecution CopyValue -> SignaledExecution CopyValue
gc val = do
  v <- val
  currSize <- gets $ view (heap . heapSize)
  maxSize <- gets $ view (heap . maxSizeBeforeGc)

  -- Run GC when heap size exceeds max.
  when (currSize > maxSize) $ do
    lift $ garbageCollect (refTrace v)

  return v

-- | Helper to add an entry to a map or a set.
hashedAddEntry :: Hashed -> a -> HM.HashMap Hashed [a] -> HM.HashMap Hashed [a]
hashedAddEntry hash val = HM.insertWith (++) hash [val]

-- | Helper to create a hashmap from a list of tuples by grouping equal keys
-- together.
groupByKeyHM :: (Eq k, Hashable k) => [(k, v)] -> HM.HashMap k [v]
groupByKeyHM xs = HM.fromListWith (++) [(k, [v]) | (k, v) <- xs]
