-- | Hash compiler program stack implementation.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Runtime.Stack where

import Control.Lens (view, (.~))
import Control.Monad (when)
import Control.Monad.State (gets, modify)
import Runtime.Boot

-- | Append a stack frame to the current stack.
addStackFrame :: StackFrame -> Execution ()
addStackFrame f = do
  size <- gets . view $ currentStackSize
  max <- gets . view $ maxStackSize
  s <- gets . view $ stack . frames

  when (size + 1 >= max) $ do
    panic "Exceeded maximum stack size! Use '-s' parameter to increase it."

  -- Set new data and size
  modify $ currentStackSize .~ size + 1
  modify $ (stack . frames) .~ f : s

-- | Pop the last stack frame from the stack.
popStackFrame :: Execution ()
popStackFrame = do
  size <- gets . view $ currentStackSize
  s <- gets . view $ stack . frames

  when (size == 0) $ do
    internalPanic "Attempting to evict frame from an empty stack."

  -- Set new data and size
  modify $ currentStackSize .~ size - 1
  modify $ (stack . frames) .~ tail s
