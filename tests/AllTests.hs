module AllTests (allTests) where

import Test.HUnit (Test (TestList), Testable (test), runTestTT)
import qualified TestAst as TA
import qualified TestIntrinsics as TI
import qualified TestLexer as TL
import qualified TestVM as TVM

allTests :: Test
allTests = TestList (TL.allTests ++ TA.allTests ++ TVM.allTests ++ TI.allTests)
