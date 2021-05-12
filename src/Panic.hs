module Panic where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Debug.Trace (traceStack)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)

-- | Internal compiler panic function (helper). This helper will trace the current
-- | execution stack (haskell) world, print the passed error message (and maybe a
-- | the execution stack in the Hash VM).
-- |
-- | Note: traceStack (or any stack tracing) doesn't work particularly well with GHC
-- | working in default mode, therefore it is required to compile the project with
-- | the '--flags debug' mode to produce meaningful stack traces (haskell). For
-- | more information check out: https://github.com/feds01/hash/pull/119
internalPanic' :: MonadIO m => String -> m () -> m a
internalPanic' msg extra =
  traceStack
    ("Sorry :^(\nInternal Panic: " ++ msg)
    ( do
        _ <- extra
        _ <- liftIO (putStrLn "\nThis is an interpreter bug, please file a bug report at\n\n\t\thttps://github.com/feds01/hash/issues\n")
        liftIO exitFailure >> error "" -- the error is unreachable, but we can't do it without since we should be returning the monad we got
    )

-- | Internal compiler panic function (pure). In an environment that is monadically
-- | independent, we still might need to crash the compiler. To do this, we use the
-- | usnsafePerformIO funnction, which is unsafe but it is ok becasue we kill the
-- | running process anyway.
internalPanicPure :: String -> a
internalPanicPure msg = unsafePerformIO $ internalPanic' msg (return ())

-- | Internal compiler panic function for unreachable code (pure).
unreachablePure :: a
unreachablePure = internalPanicPure "reached unreachable case"
