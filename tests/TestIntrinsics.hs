module TestIntrinsics (allTests) where

import Control.Exception (ErrorCall (ErrorCall), handleJust)
import Control.Monad.Cont (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Sequence as S
import Data.Text (pack)
import qualified Data.Vector as V
import qualified Runtime.Boot as H
import Runtime.Heap (heapDeref, newHeapVal)
import qualified Runtime.Intrinsics as I
import qualified Runtime.Primitives as P
import qualified Runtime.VM as VM
import Test.HUnit (Test (TestCase), Testable (test), assertBool, assertEqual, assertFailure, runTestTT)

runFunc :: H.IntrinsicFunc -> [H.CopyValue] -> IO H.CopyValue
runFunc func args =
  H.runSignaledExecution (lift $ func args) (H.emptyProgramCtx 10000)
    >>= ( \(res, _) -> case res of
            (Right r) -> return r
        )

testFuncOk :: String -> H.IntrinsicFunc -> [H.CopyValue] -> H.CopyValue -> Test
testFuncOk desc func args val =
  TestCase
    ( H.runSignaledExecution (lift $ func args) (H.emptyProgramCtx 10000)
        >>= (\(res, _) -> assertEqual desc (Right val) res)
    )

testFuncExec :: String -> H.Execution Bool -> Test
testFuncExec desc func =
  TestCase $
    H.runSignaledExecution
      (lift func)
      (H.emptyProgramCtx 10000)
      >>= ( \(res, _) -> case res of
              (Right r) -> assertBool desc r
              _ -> assertFailure "Not expecting signal from func"
          )

testFuncWithHeapOk :: String -> H.IntrinsicFunc -> [H.HeapValue] -> H.Execution H.CopyValue -> Test
testFuncWithHeapOk desc func args val =
  TestCase $
    H.runSignaledExecution
      ( do
          refs <- lift $ mapM newHeapVal args
          response <- lift $ func (map H.RefValue refs)
          expected <- lift val

          case (response, expected) of
            a@(H.RefValue l, H.RefValue r) -> compareRefs a
            (l, r) -> return $ l == r
      )
      (H.emptyProgramCtx 10000)
      >>= ( \(res, _) -> case res of
              (Right r) -> assertBool desc r
              _ -> assertFailure "Not expecting signal from func"
          )

compareRefs :: (H.CopyValue, H.CopyValue) -> H.SignaledExecution Bool
compareRefs (H.RefValue l, H.RefValue r) = do
  lhs <- lift $ heapDeref l
  rhs <- lift $ heapDeref r

  case (lhs, rhs) of
    (H.MapV l1, H.MapV r1) -> return $ l1 == r1
    (H.SetV l1, H.SetV r1) -> return $ l1 == r1
    (H.ListV l1, H.ListV r1) -> return $ l1 == r1
    (H.StructV l1, H.StructV r1) -> return $ l1 == r1
    (H.StrV l1, H.StrV r1) -> return $ l1 == r1
    (H.EnumArgs l1, H.EnumArgs r1) -> do
      -- compare the size of enum args
      if V.length l1 == V.length r1 && V.length l1 > 0
        then do
          let t = V.head l1
          let s = V.head r1

          case (t, s) of
            k@(H.RefValue _, H.RefValue _) -> do
              compareRefs k
            -- liftIO $ putStrLn $ "got from enum-args comp" ++ show b
            (l, r) -> return $ l == r
        else return False
    _ -> return $ l == r
compareRefs (l, h) = return $ l == h

testFuncWithStackOk :: String -> H.IntrinsicFunc -> [H.CopyValue] -> H.Execution H.CopyValue -> Test
testFuncWithStackOk desc func args val =
  TestCase $
    H.runSignaledExecution
      ( do
          response <- lift $ func args
          lift val >>= \derefAns -> do
            case (response, derefAns) of
              a@(H.RefValue l, H.RefValue r) -> compareRefs a
              (H.EnumV l s, H.EnumV t b) -> do
                if l == t
                  then do
                    case (s, b) of
                      (Just m, Just n) -> compareRefs (H.RefValue m, H.RefValue n)
                      _ -> return False
                  else return False
              (k, l) -> return $ k == l
      )
      (H.emptyProgramCtx 10000)
      >>= ( \(res, _) -> case res of
              (Right r) -> assertBool desc r
              _ -> assertFailure "Not expecting signal from func"
          )

-- | Make an 'Err' enum variant since intrinsics use this as a comparison value
makeErrV :: H.HeapValue -> H.Execution H.CopyValue
makeErrV item = do
  ref <- newHeapVal item
  argRef <- newHeapVal (H.EnumArgs (V.singleton (H.RefValue ref)))
  return $ H.EnumV (H.EnumVariant 5) (Just argRef)

-- | allows a heap value to be converted to a copy value in execution context
allocOnHeap :: H.HeapValue -> H.Execution H.CopyValue
allocOnHeap h = newHeapVal h >>= \ref -> return $ H.RefValue ref

-- | lift some value into execution context
withinExec :: H.CopyValue -> H.Execution H.CopyValue
withinExec = return

mathIntrinsicTests :: [Test]
mathIntrinsicTests =
  [ testFuncOk "test sqrt intrinsic with float argument (1)" I.intrinsicSqrt [H.FloatV 9] (H.FloatV 3),
    testFuncOk "test sqrt intrinsic with float argument (2)" I.intrinsicSqrt [H.FloatV 12] (H.FloatV 3.4641016151377544),
    testFuncOk "test sqrt intrinsic with float argument (3)" I.intrinsicSqrt [H.FloatV 1] (H.FloatV 1),
    testFuncOk "test floor intrinsic (1)" I.intrinsicFloor [H.FloatV 9.99999] (H.FloatV 9),
    testFuncOk "test floor intrinsic (2)" I.intrinsicFloor [H.FloatV 1.3] (H.FloatV 1),
    testFuncOk "test floor intrinsic (3)" I.intrinsicFloor [H.FloatV $ -2.7] (H.FloatV $ -3),
    testFuncOk "test ceil intrinsic (1)" I.intrinsicCeil [H.FloatV 9.123123] (H.FloatV 10),
    testFuncOk "test ceil intrinsic (2)" I.intrinsicCeil [H.FloatV 9.5] (H.FloatV 10),
    testFuncOk "test ceil intrinsic (3)" I.intrinsicCeil [H.FloatV $ -9.999999] (H.FloatV $ -9),
    testFuncOk "test round intrinsic (1)" I.intrinsicRound [H.FloatV 9] (H.FloatV 9),
    testFuncOk "test round intrinsic (2)" I.intrinsicRound [H.FloatV 9.5] (H.FloatV 10),
    testFuncOk "test round intrinsic (3)" I.intrinsicRound [H.FloatV 9.4] (H.FloatV 9),
    testFuncOk "test round intrinsic (4)" I.intrinsicRound [H.FloatV $ -9.5] (H.FloatV $ -10),
    testFuncOk "test round intrinsic (5)" I.intrinsicRound [H.FloatV $ -9.4] (H.FloatV $ -9),
    -- trig funcs
    testFuncOk "test sin intrinsic (1)" I.intrinsicSin [H.FloatV 1] (H.FloatV $ sin 1),
    testFuncOk "test sin intrinsic (2)" I.intrinsicSin [H.FloatV 0] (H.FloatV 0),
    testFuncOk "test sin intrinsic (3)" I.intrinsicSin [H.FloatV $ -1] (H.FloatV $ sin $ -1),
    testFuncOk "test cos intrinsic (1)" I.intrinsicCos [H.FloatV 1] (H.FloatV $ cos 1),
    testFuncOk "test cos intrinsic (2)" I.intrinsicCos [H.FloatV 0] (H.FloatV $ cos 0),
    testFuncOk "test cos intrinsic (3)" I.intrinsicCos [H.FloatV $ -1] (H.FloatV $ cos $ -1),
    testFuncOk "test tan intrinsic (1)" I.intrinsicTan [H.FloatV 1] (H.FloatV $ tan 1),
    testFuncOk "test tan intrinsic (2)" I.intrinsicTan [H.FloatV 0] (H.FloatV $ tan 0),
    testFuncOk "test tan intrinsic (3)" I.intrinsicTan [H.FloatV $ -1] (H.FloatV $ tan $ -1),
    -- arc-trig funcs
    testFuncOk "test asin intrinsic (1)" I.intrinsicAsin [H.FloatV 1] (H.FloatV $ asin 1),
    testFuncOk "test asin intrinsic (2)" I.intrinsicAsin [H.FloatV 0] (H.FloatV $ asin 0),
    testFuncOk "test asin intrinsic (3)" I.intrinsicAsin [H.FloatV $ -1] (H.FloatV (asin $ -1)),
    testFuncOk "test acos intrinsic (1)" I.intrinsicAcos [H.FloatV 1] (H.FloatV $ acos 1),
    testFuncOk "test acos intrinsic (2)" I.intrinsicAcos [H.FloatV 0] (H.FloatV $ acos 0),
    testFuncOk "test acos intrinsic (3)" I.intrinsicAcos [H.FloatV $ -1] (H.FloatV $ acos $ -1),
    testFuncOk "test atan intrinsic (1)" I.intrinsicAtan [H.FloatV 1] (H.FloatV $ atan 1),
    testFuncOk "test atan intrinsic (2)" I.intrinsicAtan [H.FloatV 0] (H.FloatV $ atan 0),
    testFuncOk "test atan intrinsic (3)" I.intrinsicAtan [H.FloatV $ -1] (H.FloatV $ atan $ -1),
    testFuncOk "test sinh intrinsic (1)" I.intrinsicSinh [H.FloatV 1] (H.FloatV $ sinh 1),
    testFuncOk "test sinh intrinsic (2)" I.intrinsicSinh [H.FloatV 0] (H.FloatV $ sinh 0),
    testFuncOk "test sinh intrinsic (3)" I.intrinsicSinh [H.FloatV $ -1] (H.FloatV $ sinh $ -1),
    testFuncOk "test cosh intrinsic (1)" I.intrinsicCosh [H.FloatV 1] (H.FloatV $ cosh 1),
    testFuncOk "test cosh intrinsic (2)" I.intrinsicCosh [H.FloatV 0] (H.FloatV $ cosh 0),
    testFuncOk "test cosh intrinsic (3)" I.intrinsicCosh [H.FloatV $ -1] (H.FloatV $ cosh $ -1),
    testFuncOk "test tanh intrinsic (1)" I.intrinsicTanh [H.FloatV 1] (H.FloatV $ tanh 1),
    testFuncOk "test tanh intrinsic (2)" I.intrinsicTanh [H.FloatV 0] (H.FloatV $ tanh 0),
    testFuncOk "test tanh intrinsic (3)" I.intrinsicTanh [H.FloatV $ -1] (H.FloatV $ tanh $ -1),
    testFuncOk "test asinh intrinsic (1)" I.intrinsicAsinh [H.FloatV 1] (H.FloatV $ asinh 1),
    testFuncOk "test asinh intrinsic (2)" I.intrinsicAsinh [H.FloatV 0] (H.FloatV $ asinh 0),
    testFuncOk "test asinh intrinsic (3)" I.intrinsicAsinh [H.FloatV $ -1] (H.FloatV $ asinh $ -1),
    testFuncOk "test acosh intrinsic (1)" I.intrinsicAcosh [H.FloatV 1] (H.FloatV $ acosh 1),
    testFuncOk "test atanh intrinsic (1)" I.intrinsicAtanh [H.FloatV 1] (H.FloatV $ atanh 1),
    testFuncOk "test atanh intrinsic (2)" I.intrinsicAtanh [H.FloatV 0] (H.FloatV $ atanh 0),
    testFuncOk "test atanh intrinsic (3)" I.intrinsicAtanh [H.FloatV $ -1] (H.FloatV $ atanh $ -1),
    --exponential funcs
    testFuncOk "test log intrinsic (1)" I.intrinsicLog [H.FloatV 4, H.FloatV 2] (H.FloatV 2),
    testFuncOk "test log intrinsic (2)" I.intrinsicLog [H.FloatV 2, H.FloatV 4] (H.FloatV 0.5),
    testFuncOk "test pow intrinsic (1)" I.intrinsicPow [H.FloatV 4, H.FloatV 2] (H.FloatV 16),
    testFuncOk "test pow intrinsic (2)" I.intrinsicPow [H.FloatV 2, H.FloatV 4] (H.FloatV 16),
    testFuncOk "test pow intrinsic (3)" I.intrinsicPow [H.FloatV 2, H.FloatV $ -4] (H.FloatV 0.0625),
    testFuncOk "test pow intrinsic (4)" I.intrinsicPow [H.FloatV $ -2, H.FloatV 4] (H.FloatV 16),
    testFuncOk "test pow intrinsic (5)" I.intrinsicPow [H.FloatV $ -2, H.FloatV 3] (H.FloatV $ -8.0),
    testFuncOk "test pow intrinsic (6)" I.intrinsicPow [H.FloatV $ -2, H.FloatV $ -3] (H.FloatV $ -0.125)
  ]

-- | Tests for the 'conv()' intrinsic family
convIntrinsicTests :: [Test]
convIntrinsicTests =
  [ -- float conv
    testFuncWithStackOk "test conv->float (1)" I.intrinsicConvToFloat [H.IntV 64] (P.resultOkV $ H.FloatV 64),
    testFuncWithStackOk "test conv->float (2)" I.intrinsicConvToFloat [H.IntV (-10)] (P.resultOkV $ H.FloatV $ -10),
    testFuncWithStackOk "test conv->float (3)" I.intrinsicConvToFloat [H.FloatV 10] (P.resultOkV $ H.FloatV 10),
    testFuncWithStackOk "test conv->float (4)" I.intrinsicConvToFloat [H.FloatV (-10)] (P.resultOkV $ H.FloatV $ -10),
    --testFuncWithHeapOk "test conv->float (5)" I.intrinsicConvToFloat [H.StrV (pack "10")] (P.resultOkV $ H.FloatV 10),
    --testFuncWithHeapOk "test conv->float (6)" I.intrinsicConvToFloat [H.StrV (pack "-10")] (P.resultOkV $ H.FloatV $ -10),

    -- char conv
    testFuncWithStackOk "test conv->char (1)" I.intrinsicConvToChar [H.IntV 97] (P.resultOkV $ H.CharV 'a'),
    testFuncWithStackOk "test conv->char (2)" I.intrinsicConvToChar [H.IntV (-10)] (makeErrV $ H.StrV (pack "Integer Literal overflow, cannot cast to char!")),
    testFuncWithStackOk "test conv->char (3)" I.intrinsicConvToChar [H.CharV 'a'] (P.resultOkV $ H.CharV 'a'),
    -- int conv
    testFuncWithStackOk "test conv->int (1)" I.intrinsicConvToInt [H.IntV 33] (P.resultOkV $ H.IntV 33),
    testFuncWithStackOk "test conv->int (2)" I.intrinsicConvToInt [H.IntV $ -33] (P.resultOkV $ H.IntV $ -33),
    testFuncWithStackOk "test conv->int (3)" I.intrinsicConvToInt [H.FloatV 19] (P.resultOkV $ H.IntV 19),
    testFuncWithStackOk "test conv->int (4)" I.intrinsicConvToInt [H.FloatV $ -19] (P.resultOkV $ H.IntV $ -19),
    testFuncWithStackOk "test conv->int (5)" I.intrinsicConvToInt [H.CharV 'a'] (P.resultOkV $ H.IntV 97),
    testFuncWithStackOk "test conv->int (6)" I.intrinsicConvToInt [H.CharV '\n'] (P.resultOkV $ H.IntV 10),
    --testFuncWithHeapOk "test conv->int (7)" I.intrinsicConvToInt [H.StrV (pack "1")] (P.resultOkV $ H.IntV 1),
    --testFuncWithHeapOk "test conv->int (8)" I.intrinsicConvToInt [H.StrV (pack "-1")] (P.resultOkV $ H.IntV $ -1)

    --str conv
    testFuncWithHeapOk "test conv->string (1)" I.intrinsicConvToStr [H.StrV (pack "abc")] (allocOnHeap $ H.StrV (pack "abc"))
    --testFuncWithStackOk "test conv->string (2)" I.intrinsicConvToStr [H.IntV 1] (allocOnHeap $ H.StrV (pack "1"))
    --testFuncWithHeapOk "test bracket->string conv (1)" I.intrinsicConvBrackettedToStr [H.ListV [H.IntV 1, H.IntV 2, H.IntV 3]] (allocOnHeap $ H.StrV (pack "123"))
  ]

-- | Tests for the list/set/map operation intrinsics family
bracketOpIntrinsicTests :: [Test]
bracketOpIntrinsicTests =
  []

-- | Tests for the intrinsic operators including all of the primitive
-- | operations such as '+', '-', etc.
-- | @@Improvement: use quickchecks for these
operatorIntrinsicTests :: [Test]
operatorIntrinsicTests =
  [ -- equality tests
    testFuncWithHeapOk "test equality string (1)" I.intrinsicEq [H.StrV (pack "abc"), H.StrV (pack "abc")] (withinExec $ P.boolV True),
    testFuncOk "test equality int (1)" I.intrinsicEq [H.IntV 64, H.IntV 2] (P.boolV False),
    testFuncOk "test equality int (2)" I.intrinsicEq [H.IntV (-10), H.IntV (-10)] (P.boolV True),
    testFuncOk "test equality int (3)" I.intrinsicEq [H.IntV 10, H.IntV (-10)] (P.boolV False),
    testFuncOk "test equality float (1)" I.intrinsicEq [H.FloatV 64, H.FloatV 64] (P.boolV True),
    testFuncOk "test equality float (2)" I.intrinsicEq [H.FloatV 64, H.FloatV 12] (P.boolV False),
    testFuncOk "test equality float (3)" I.intrinsicEq [H.FloatV 64, H.FloatV 64.0] (P.boolV True),
    testFuncOk "test equality char (1)" I.intrinsicEq [H.CharV 'a', H.CharV 'a'] (P.boolV True),
    testFuncOk "test equality char (2)" I.intrinsicEq [H.CharV 'a', H.CharV 'b'] (P.boolV False),
    testFuncWithHeapOk "test equality string (1)" I.intrinsicEq [H.StrV (pack "abc"), H.StrV (pack "abc")] (withinExec $ P.boolV True),
    testFuncWithHeapOk "test equality string (1)" I.intrinsicEq [H.StrV (pack "abc"), H.StrV (pack "cba")] (withinExec $ P.boolV False),
    -- ref tests
    -- operator tests
    testFuncOk "test add operator (1)" I.intrinsicAdd [H.IntV 64, H.IntV 2] (H.IntV 66),
    testFuncOk "test add operator (2)" I.intrinsicAdd [H.IntV 64, H.IntV 12] (H.IntV 76),
    testFuncOk "test add operator (3)" I.intrinsicAdd [H.IntV 64, H.IntV (-10)] (H.IntV 54),
    testFuncOk "test add operator (4)" I.intrinsicAdd [H.IntV (-64), H.IntV (-10)] (H.IntV $ -74),
    testFuncOk "test add (f) operator (1)" I.intrinsicAdd [H.FloatV 64, H.FloatV 2] (H.FloatV 66),
    testFuncOk "test add (f) operator (2)" I.intrinsicAdd [H.FloatV 64, H.FloatV 12] (H.FloatV 76),
    testFuncOk "test add (f) operator (3)" I.intrinsicAdd [H.FloatV 64, H.FloatV (-2)] (H.FloatV 62.0),
    testFuncOk "test add (f) operator (4)" I.intrinsicAdd [H.FloatV (-64), H.FloatV (-12)] (H.FloatV $ -76.0),
    testFuncWithHeapOk "test add (s) operator (1)" I.intrinsicAdd [H.StrV (pack "ab"), H.StrV (pack "cd")] (allocOnHeap (H.StrV (pack "abcd"))),
    testFuncWithHeapOk "test add (s) operator (2)" I.intrinsicAdd [H.StrV (pack "Hello"), H.StrV (pack " world")] (allocOnHeap (H.StrV (pack "Hello world"))),
    testFuncOk "test sub operator (1)" I.intrinsicSub [H.IntV 64, H.IntV 9] (H.IntV 55),
    testFuncOk "test sub operator (2)" I.intrinsicSub [H.IntV 64, H.IntV 2] (H.IntV 62),
    testFuncOk "test sub operator (3)" I.intrinsicSub [H.IntV (-2), H.IntV 3] (H.IntV (-5)),
    testFuncOk "test sub operator (4)" I.intrinsicSub [H.IntV (-22), H.IntV (-10)] (H.IntV (-12)),
    testFuncOk "test sub operator (5)" I.intrinsicSub [H.FloatV 6, H.FloatV 9] (H.FloatV $ -3.0),
    testFuncOk "test sub operator (6)" I.intrinsicSub [H.FloatV 32, H.FloatV 16] (H.FloatV 16),
    testFuncOk "test sub operator (7)" I.intrinsicSub [H.FloatV (-9), H.FloatV 13] (H.FloatV $ -22.0),
    testFuncOk "test sub operator (8)" I.intrinsicSub [H.FloatV (-50), H.FloatV (-20)] (H.FloatV $ -30.0),
    testFuncOk "test div operator (1)" I.intrinsicDiv [H.IntV 64, H.IntV 9] (H.IntV 7),
    testFuncOk "test div operator (2)" I.intrinsicDiv [H.IntV 64, H.IntV 2] (H.IntV 32),
    testFuncOk "test div operator (3)" I.intrinsicDiv [H.IntV (-64), H.IntV 2] (H.IntV $ -32),
    testFuncOk "test div operator (4)" I.intrinsicDiv [H.IntV (-64), H.IntV (-2)] (H.IntV 32),
    testFuncOk "test div operator (5)" I.intrinsicDiv [H.FloatV 64, H.FloatV 9] (H.FloatV 7.111111111111111),
    testFuncOk "test div operator (6)" I.intrinsicDiv [H.FloatV 64, H.FloatV 2] (H.FloatV 32),
    testFuncOk "test div operator (7)" I.intrinsicDiv [H.FloatV (-64), H.FloatV 2] (H.FloatV $ -32.0),
    testFuncOk "test div operator (8)" I.intrinsicDiv [H.FloatV (-15), H.FloatV (-2)] (H.FloatV 7.5),
    testFuncOk "test mul operator (1)" I.intrinsicMul [H.IntV 64, H.IntV $ -9] (H.IntV $ -576),
    testFuncOk "test mul operator (2)" I.intrinsicMul [H.IntV $ -64, H.IntV $ -9] (H.IntV 576),
    testFuncOk "test mul operator (3)" I.intrinsicMul [H.IntV 64, H.IntV 2] (H.IntV 128),
    testFuncOk "test mul operator (4)" I.intrinsicMul [H.IntV 64, H.IntV 9] (H.IntV 576),
    testFuncOk "test mul operator (5)" I.intrinsicMul [H.FloatV 64, H.FloatV 2] (H.FloatV 128.0),
    testFuncOk "test mul operator (6)" I.intrinsicMul [H.FloatV 64, H.FloatV 9] (H.FloatV 576.0),
    testFuncOk "test mul operator (7)" I.intrinsicMul [H.FloatV 64, H.FloatV $ -9] (H.FloatV $ -576.0),
    testFuncOk "test mul operator (8)" I.intrinsicMul [H.FloatV $ -64, H.FloatV $ -9] (H.FloatV 576.0),
    testFuncOk "test neg operator (1)" I.intrinsicNeg [H.IntV $ -64] (H.IntV 64),
    testFuncOk "test neg operator (2)" I.intrinsicNeg [H.IntV 64] (H.IntV $ -64),
    testFuncOk "test neg operator (3)" I.intrinsicNeg [H.FloatV 64] (H.FloatV $ -64),
    testFuncOk "test neg operator (3)" I.intrinsicNeg [H.FloatV 0] (H.FloatV 0),
    testFuncOk "test mod (1)" I.intrinsicMod [H.IntV 64, H.IntV 13] (H.IntV 12),
    testFuncOk "test mod (2)" I.intrinsicMod [H.IntV 64, H.IntV 9] (H.IntV 1),
    testFuncOk "test mod (3)" I.intrinsicMod [H.IntV 64, H.IntV 2] (H.IntV 0),
    testFuncOk "test mod (4)" I.intrinsicMod [H.IntV $ -5, H.IntV 3] (H.IntV 1),
    testFuncOk "test mod (f) (1)" I.intrinsicMod [H.FloatV 64, H.FloatV 9.3] (H.FloatV 8.199999999999996),
    testFuncOk "test mod (f) (2)" I.intrinsicMod [H.FloatV 64, H.FloatV 2] (H.FloatV 0),
    testFuncOk "test mod (f) (3)" I.intrinsicMod [H.FloatV $ -5, H.FloatV 2] (H.FloatV 1.0),
    -- bitwise operator tests
    testFuncOk "test div shift right (1)" I.intrinsicBitShr [H.IntV 64, H.IntV 2] (H.IntV 16),
    testFuncOk "test bit shift right (2)" I.intrinsicBitShr [H.IntV 64, H.IntV 9] (H.IntV 0),
    testFuncOk "test bit shift left (1)" I.intrinsicBitShl [H.IntV 64, H.IntV 2] (H.IntV 256),
    testFuncOk "test bit shift left (2)" I.intrinsicBitShl [H.IntV 64, H.IntV 9] (H.IntV 32768),
    testFuncOk "test bit not (1)" I.intrinsicBitNot [H.IntV 35] (H.IntV (-36)),
    testFuncOk "test bit not (2)" I.intrinsicBitNot [H.IntV 17] (H.IntV (-18)),
    testFuncOk "test bit and (1)" I.intrinsicBitAnd [H.IntV 3, H.IntV 2] (H.IntV 2),
    testFuncOk "test bit and (2)" I.intrinsicBitAnd [H.IntV 8, H.IntV 4] (H.IntV 0),
    testFuncOk "test bit or (1)" I.intrinsicBitOr [H.IntV 64, H.IntV 64] (H.IntV 64),
    testFuncOk "test bit or (2)" I.intrinsicBitOr [H.IntV 8, H.IntV 4] (H.IntV 12),
    testFuncOk "test bit xor (1)" I.intrinsicBitXor [H.IntV 2, H.IntV 2] (H.IntV 0),
    testFuncOk "test bit xor (2)" I.intrinsicBitXor [H.IntV 7, H.IntV 9] (H.IntV 14)
  ]

-- | Generic intrinsic function tests
genericIntrinsicTests :: [Test]
genericIntrinsicTests =
  [ testFuncOk "test hashing function for various int 1" I.intrinsicHash [H.IntV 3] (H.IntV 3),
    testFuncOk "test hashing function for various int 2" I.intrinsicHash [H.IntV 1000] (H.IntV 1000),
    testFuncOk "test hashing function for various float 1" I.intrinsicHash [H.FloatV 3] (H.IntV 4613937818241073152),
    testFuncOk "test hashing function for various float 2" I.intrinsicHash [H.FloatV 1000] (H.IntV 4652007308841189376),
    testFuncOk "test hashing function for various char 1" I.intrinsicHash [H.CharV 'a'] (H.IntV 97),
    testFuncOk "test hashing function for various char 2" I.intrinsicHash [H.CharV '\n'] (H.IntV 10),
    testFuncWithHeapOk "test intrinsic size for string 1" I.intrinsicSize [H.StrV (pack "abc")] (withinExec $ H.IntV 3),
    testFuncWithHeapOk "test intrinsic size for string 2" I.intrinsicSize [H.StrV (pack "")] (withinExec $ H.IntV 0),
    testFuncWithHeapOk "test intrinsic size for list 1" I.intrinsicSize [H.ListV $ S.fromList [H.IntV 1]] (withinExec $ H.IntV 1),
    testFuncWithHeapOk "test intrinsic size for list 2" I.intrinsicSize [H.ListV $ S.fromList [H.IntV 1, H.IntV 11, H.IntV 111]] (withinExec $ H.IntV 3)
  ]

allTests :: [Test]
allTests =
  concat
    [ mathIntrinsicTests,
      convIntrinsicTests,
      bracketOpIntrinsicTests,
      operatorIntrinsicTests,
      genericIntrinsicTests
    ]
