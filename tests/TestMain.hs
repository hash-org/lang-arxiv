-- | Hash Compiler types and primitives for type checking.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Main where

import AllTests (allTests)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
import TrivialParserTests

main :: IO ()
main = do
  -- now run the quickcheck tests if the hunits succeed.
  runTests >>= \case
    True -> do
      results <- runTestTT allTests
      if failures results + errors results == 0
        then exitSuccess
        else exitWith (ExitFailure 1)
    False -> exitFailure
