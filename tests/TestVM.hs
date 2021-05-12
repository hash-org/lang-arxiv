-- | Hash Compiler types and primitives for type checking.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module TestVM (allTests) where

import Control.Exception (ErrorCall (ErrorCall), handleJust)
import qualified Data.Vector as V
import qualified Runtime.Boot as H
import qualified Runtime.Primitives as P
import qualified Runtime.VM as VM
import Test.HUnit (Test (TestCase), assertEqual, assertFailure)

testEvalOk :: String -> H.CopyValue -> H.Instruction -> Test
testEvalOk desc val ins =
  TestCase
    ( H.runSignaledExecution (VM.evalInstruction ins) (H.emptyProgramCtx 10000)
        >>= (\(res, _) -> assertEqual desc (Right val) res)
    )

literalTests :: [Test]
literalTests =
  [ testEvalOk "literal int" (H.IntV 1) (H.ILiteral (H.IntV 1)),
    testEvalOk "literal float" (H.FloatV 1.0) (H.ILiteral (H.FloatV 1.0)),
    testEvalOk "literal bool" P.boolTrueV (H.ILiteral P.boolTrueV),
    testEvalOk "literal enum" (H.EnumV (H.EnumVariant 3) (Just (H.Ref 1))) (H.IEnum (H.EnumVariant 3) (V.singleton $ H.ILiteral $ H.IntV 3)),
    testEvalOk "literal char" (H.CharV 'a') (H.ILiteral (H.CharV 'a'))
  ]

bindTests :: [Test]
bindTests =
  [ testEvalOk "bind" (H.IntV 42) (H.ISeq [H.IBind (H.Name 10) (H.ILiteral (H.IntV 42)), H.INameRef (H.Name 10)]),
    testEvalOk "bind twice" (H.IntV 42) (H.ISeq [H.IBind (H.Name 10) (H.ILiteral (H.IntV 42)), H.IBind (H.Name 11) (H.INameRef (H.Name 10)), H.INameRef (H.Name 11)]),
    testEvalOk "bind duplicate" (H.IntV 42) (H.ISeq [H.IBind (H.Name 10) (H.ILiteral (H.IntV 42)), H.IBind (H.Name 11) (H.INameRef (H.Name 10)), H.INameRef (H.Name 11)])
  ]

loopTests :: [Test]
loopTests =
  [ testEvalOk
      "count to 42"
      ( H.TupleV $
          V.fromList
            [ H.IntV 42,
              H.IntV 42,
              H.IntV 42,
              H.IntV 42,
              H.IntV 42,
              H.IntV 42
            ]
      )
      ( H.ISeq
          [ H.IBind
              (H.Name 5)
              ( H.IFnDef
                  "count"
                  [H.Name 10]
                  ( H.ISeq
                      [ H.ILoop
                          ( H.IBlock
                              ( H.IMatch
                                  (H.INameRef (H.Name 10))
                                  [ (H.IntP 1, H.ISeq [H.INameSet (H.Name 10) (H.ILiteral (H.IntV 2))]),
                                    (H.IntP 2, H.ISeq [H.INameSet (H.Name 10) (H.ILiteral (H.IntV 3))]),
                                    (H.IntP 3, H.ISeq [H.INameSet (H.Name 10) (H.ILiteral (H.IntV 4))]),
                                    (H.IntP 4, H.ISeq [H.INameSet (H.Name 10) (H.ILiteral (H.IntV 5))]),
                                    (H.IntP 5, H.ISeq [H.INameSet (H.Name 10) (H.ILiteral (H.IntV 6))]),
                                    (H.IgnoreP, H.ISeq [H.INameSet (H.Name 10) (H.ILiteral (H.IntV 42)), H.IBreak])
                                  ]
                              )
                          ),
                        H.IReturn (H.INameRef (H.Name 10))
                      ]
                  )
              ),
            H.ITuple $
              V.fromList
                [ H.IFnCall "interactive" (H.INameRef (H.Name 5)) [H.ILiteral (H.IntV 1)],
                  H.IFnCall "interactive" (H.INameRef (H.Name 5)) [H.ILiteral (H.IntV 2)],
                  H.IFnCall "interactive" (H.INameRef (H.Name 5)) [H.ILiteral (H.IntV 3)],
                  H.IFnCall "interactive" (H.INameRef (H.Name 5)) [H.ILiteral (H.IntV 4)],
                  H.IFnCall "interactive" (H.INameRef (H.Name 5)) [H.ILiteral (H.IntV 5)],
                  H.IFnCall "interactive" (H.INameRef (H.Name 5)) [H.ILiteral (H.IntV 6)]
                ]
          ]
      ),
    testEvalOk "immediately break" H.VoidV (H.ILoop H.IBreak),
    testEvalOk
      "continue, then break"
      P.boolTrueV
      ( H.ISeq
          [ H.IBind (H.Name 5) (H.ILiteral P.boolFalseV),
            H.ILoop
              ( H.IMatch
                  (H.INameRef (H.Name 5))
                  [ (H.EnumP (H.EnumVariant 0) [], H.INameSet (H.Name 5) (H.ILiteral P.boolTrueV)),
                    (H.EnumP (H.EnumVariant 1) [], H.IBreak)
                  ]
              ),
            H.INameRef (H.Name 5)
          ]
      ),
    testEvalOk
      "continue, then break"
      P.boolTrueV
      ( H.ISeq
          [ H.IBind (H.Name 5) (H.ILiteral P.boolFalseV),
            H.ILoop
              ( H.IMatch
                  (H.INameRef (H.Name 5))
                  [ (H.EnumP (H.EnumVariant 0) [], H.INameSet (H.Name 5) (H.ILiteral P.boolTrueV)),
                    (H.EnumP (H.EnumVariant 1) [], H.IBreak)
                  ]
              ),
            H.INameRef (H.Name 5)
          ]
      )
  ]

-- | Combine all the tests ready to run them
allTests :: [Test]
allTests = literalTests ++ bindTests ++ loopTests
