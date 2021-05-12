module TestAst where

import Control.Monad.State (State, runState)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.Void (Void)
import Debug.Trace (trace)
import qualified Parse.Ast as A
import qualified Parse.Boot as PB
import qualified Parse.Lexer as L
import Test.HUnit (Test (TestCase), assertEqual, assertFailure)
import TestUtils (runParserWithState)
import qualified Text.Megaparsec as M

-- | Create an AST node with the given data for testing.
astNode :: Int -> Int -> a -> PB.AstNode a
astNode begin width inner = PB.AstNode {PB.body = inner, PB.offsetBegin = begin - 1, PB.offsetEnd = begin + width - 1, PB.filename = ""}

-- | Function that expects a parser to return OK and an AST.
testAstOk :: Eq a => Show a => String -> String -> PB.Parser (PB.AstNode a) -> a -> Test
testAstOk desc inp parser out =
  TestCase
    ( assertEqual
        desc
        (Right out)
        (PB.body <$> runParserWithState parser (A.emptyModuleContext "<interactive>") inp)
    )

-- | Function that expects a parser to fail.
testAstFail :: Eq a => Show a => String -> String -> PB.Parser a -> Test
testAstFail desc inp parser =
  TestCase
    ( case runParserWithState parser (A.emptyModuleContext "<interactive>") inp of
        Right a -> assertFailure (msg a)
        Left a -> return ()
    )
  where
    msg a =
      (if null desc then "" else desc ++ "\n")
        ++ "expected failure but got: "
        ++ show a

-- | Test AccessName/Name parsing
basicTests :: [Test]
basicTests =
  [ testAstOk "test name" "Hello" A.pName (PB.Name "Hello"),
    testAstOk "test access name" "greetings" A.pAccessName (PB.AccessName [astNode 1 9 (PB.Name "greetings")]),
    testAstOk "test qualified name" "a::b::c" A.pAccessName (PB.AccessName [astNode 1 1 (PB.Name "a"), astNode 4 1 (PB.Name "b"), astNode 7 1 (PB.Name "c")])
  ]

funcCallArgs :: Maybe [PB.AstNode PB.Type] -> [PB.AstNode PB.Expression] -> PB.FunctionCallArgs
funcCallArgs a b = PB.FunctionCallArgs a b ""

-- | Test type arguments and annotations parsing
typeTests :: [Test]
typeTests =
  [ testAstOk "test type var parsing" "A" A.pType (PB.TypeVar (astNode 1 1 (PB.Name "A"))),
    testAstOk "test named type parsing" "Hello" A.pType (PB.NamedType (astNode 1 5 (PB.AccessName [astNode 1 5 (PB.Name "Hello")])) []),
    testAstOk "test primitive type parsing" "i32" A.pType (PB.NamedType (astNode 1 3 (PB.AccessName [astNode 1 3 (PB.Name "i32")])) []),
    testAstOk
      "test generic type parsing 1"
      "io::Result<i32, void>"
      A.pType
      ( PB.NamedType
          (astNode 1 10 (PB.AccessName [astNode 1 2 (PB.Name "io"), astNode 5 6 (PB.Name "Result")]))
          [ astNode 12 3 (PB.NamedType (astNode 12 3 (PB.AccessName [astNode 12 3 (PB.Name "i32")])) []),
            astNode 17 4 (PB.NamedType (astNode 17 4 (PB.AccessName [astNode 17 4 (PB.Name "void")])) [])
          ]
      ),
    testAstOk
      "test generic type parsing 2"
      "my::TypeOne<my::TypeTwo<my::TypeThree>>"
      A.pType
      ( PB.NamedType
          (astNode 1 11 (PB.AccessName [astNode 1 2 (PB.Name "my"), astNode 5 7 (PB.Name "TypeOne")]))
          [ astNode
              13
              26
              ( PB.NamedType
                  (astNode 13 11 (PB.AccessName [astNode 13 2 (PB.Name "my"), astNode 17 7 (PB.Name "TypeTwo")]))
                  [ astNode
                      25
                      13
                      ( PB.NamedType
                          (astNode 25 13 (PB.AccessName [astNode 25 2 (PB.Name "my"), astNode 29 9 (PB.Name "TypeThree")]))
                          []
                      )
                  ]
              )
          ]
      ),
    testAstOk
      "test generic type parsing 3"
      "Long<one, two, three, four, T>"
      A.pType
      ( PB.NamedType
          (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Long")]))
          [ astNode 6 3 (PB.NamedType (astNode 6 3 (PB.AccessName [astNode 6 3 (PB.Name "one")])) []),
            astNode 11 3 (PB.NamedType (astNode 11 3 (PB.AccessName [astNode 11 3 (PB.Name "two")])) []),
            astNode 16 5 (PB.NamedType (astNode 16 5 (PB.AccessName [astNode 16 5 (PB.Name "three")])) []),
            astNode 23 4 (PB.NamedType (astNode 23 4 (PB.AccessName [astNode 23 4 (PB.Name "four")])) []),
            astNode 29 1 (PB.TypeVar (astNode 29 1 (PB.Name "T")))
          ]
      ),
    testAstOk
      "test generic type parsing infer"
      "Long<one, two, three, four, _>"
      A.pType
      ( PB.NamedType
          (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Long")]))
          [ astNode 6 3 (PB.NamedType (astNode 6 3 (PB.AccessName [astNode 6 3 (PB.Name "one")])) []),
            astNode 11 3 (PB.NamedType (astNode 11 3 (PB.AccessName [astNode 11 3 (PB.Name "two")])) []),
            astNode 16 5 (PB.NamedType (astNode 16 5 (PB.AccessName [astNode 16 5 (PB.Name "three")])) []),
            astNode 23 4 (PB.NamedType (astNode 23 4 (PB.AccessName [astNode 23 4 (PB.Name "four")])) []),
            astNode 29 1 PB.InferType
          ]
      ),
    testAstFail
      "test generic trailing comma (1)"
      "Some<one,>"
      A.pType,
    testAstFail
      "test generic trailing comma (2)"
      "Some<one,two,>"
      A.pType,
    testAstOk
      "test existential type"
      "Long<one, two, three, four, ?>"
      A.pType
      ( PB.NamedType
          (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Long")]))
          [ astNode 6 3 (PB.NamedType (astNode 6 3 (PB.AccessName [astNode 6 3 (PB.Name "one")])) []),
            astNode 11 3 (PB.NamedType (astNode 11 3 (PB.AccessName [astNode 11 3 (PB.Name "two")])) []),
            astNode 16 5 (PB.NamedType (astNode 16 5 (PB.AccessName [astNode 16 5 (PB.Name "three")])) []),
            astNode 23 4 (PB.NamedType (astNode 23 4 (PB.AccessName [astNode 23 4 (PB.Name "four")])) []),
            astNode 29 1 PB.ExistentialType
          ]
      ),
    testAstFail
      "test set type parsing 1"
      "{}"
      A.pType,
    testAstFail
      "test set type parsing 2"
      "{,}"
      A.pType,
    testAstOk
      "test set type parsing 3"
      "{A}"
      A.pType
      (PB.NamedType (astNode 1 3 (PB.AccessName [astNode 1 3 (PB.Name "Set")])) [astNode 2 1 (PB.TypeVar (astNode 2 1 (PB.Name "A")))]),
    testAstOk
      "test set type parsing 4"
      "{{str}}"
      A.pType
      ( PB.NamedType
          (astNode 1 7 (PB.AccessName [astNode 1 7 (PB.Name "Set")]))
          [ astNode
              2
              5
              ( PB.NamedType
                  ( astNode
                      2
                      5
                      (PB.AccessName [astNode 2 5 (PB.Name "Set")])
                  )
                  [astNode 3 3 (PB.NamedType (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "str")])) [])]
              )
          ]
      ),
    testAstOk
      "test set type parsing 5"
      "{io::Result<u32, {str}>}"
      A.pType
      ( PB.NamedType
          (astNode 1 24 (PB.AccessName [astNode 1 24 (PB.Name "Set")]))
          [ astNode
              2
              22
              ( PB.NamedType
                  (astNode 2 10 (PB.AccessName [astNode 2 2 (PB.Name "io"), astNode 6 6 (PB.Name "Result")]))
                  [ astNode 13 3 (PB.NamedType (astNode 13 3 (PB.AccessName [astNode 13 3 (PB.Name "u32")])) []),
                    astNode
                      18
                      5
                      ( PB.NamedType
                          (astNode 18 5 (PB.AccessName [astNode 18 5 (PB.Name "Set")]))
                          [astNode 19 3 (PB.NamedType (astNode 19 3 (PB.AccessName [astNode 19 3 (PB.Name "str")])) [])]
                      )
                  ]
              )
          ]
      ),
    testAstFail
      "test list type parsing 1"
      "[]"
      A.pType,
    testAstFail
      "test list type parsing 2"
      "[,]"
      A.pType,
    testAstFail
      "test list type parsing 3"
      "[A,B]"
      A.pType,
    testAstOk
      "test list type parsing 4"
      "[A]"
      A.pType
      (PB.NamedType (astNode 1 3 (PB.AccessName [astNode 1 3 (PB.Name "List")])) [astNode 2 1 (PB.TypeVar (astNode 2 1 (PB.Name "A")))]),
    testAstOk
      "test list type parsing 5"
      "[[str]]"
      A.pType
      ( PB.NamedType
          (astNode 1 7 (PB.AccessName [astNode 1 7 (PB.Name "List")]))
          [ astNode
              2
              5
              ( PB.NamedType
                  ( astNode
                      2
                      5
                      (PB.AccessName [astNode 2 5 (PB.Name "List")])
                  )
                  [astNode 3 3 (PB.NamedType (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "str")])) [])]
              )
          ]
      ),
    testAstOk
      "test list type parsing 6"
      "[io::Result<u32, [str]>]"
      A.pType
      ( PB.NamedType
          (astNode 1 24 (PB.AccessName [astNode 1 24 (PB.Name "List")]))
          [ astNode
              2
              22
              ( PB.NamedType
                  (astNode 2 10 (PB.AccessName [astNode 2 2 (PB.Name "io"), astNode 6 6 (PB.Name "Result")]))
                  [ astNode 13 3 (PB.NamedType (astNode 13 3 (PB.AccessName [astNode 13 3 (PB.Name "u32")])) []),
                    astNode
                      18
                      5
                      ( PB.NamedType
                          (astNode 18 5 (PB.AccessName [astNode 18 5 (PB.Name "List")]))
                          [astNode 19 3 (PB.NamedType (astNode 19 3 (PB.AccessName [astNode 19 3 (PB.Name "str")])) [])]
                      )
                  ]
              )
          ]
      ),
    testAstFail
      "test map type parsing 1"
      "{A:}"
      A.pType,
    testAstFail
      "test map type parsing 2"
      "{:B}"
      A.pType,
    testAstFail
      "test map type parsing 3"
      "{:}"
      A.pType,
    testAstOk
      "test map type parsing 4"
      "{A:B}"
      A.pType
      ( PB.NamedType
          (astNode 1 5 (PB.AccessName [astNode 1 5 (PB.Name "Map")]))
          [ astNode 2 1 (PB.TypeVar (astNode 2 1 (PB.Name "A"))),
            astNode 4 1 (PB.TypeVar (astNode 4 1 (PB.Name "B")))
          ]
      ),
    testAstOk
      "test list type parsing 5"
      "{{X:Y}:{Z:W}}"
      A.pType
      ( PB.NamedType
          ( astNode
              1
              13
              (PB.AccessName [astNode 1 13 (PB.Name "Map")])
          )
          [ astNode
              2
              5
              ( PB.NamedType
                  ( astNode
                      2
                      5
                      ( PB.AccessName
                          [ astNode 2 5 (PB.Name "Map")
                          ]
                      )
                  )
                  [astNode 3 1 (PB.TypeVar (astNode 3 1 (PB.Name "X"))), astNode 5 1 (PB.TypeVar (astNode 5 1 (PB.Name "Y")))]
              ),
            astNode
              8
              5
              ( PB.NamedType
                  ( astNode
                      8
                      5
                      ( PB.AccessName
                          [ astNode 8 5 (PB.Name "Map")
                          ]
                      )
                  )
                  [astNode 9 1 (PB.TypeVar (astNode 9 1 (PB.Name "Z"))), astNode 11 1 (PB.TypeVar (astNode 11 1 (PB.Name "W")))]
              )
          ]
      ),
    testAstOk
      "test set type parsing 6"
      "{io::Result<S::u32, F::u64>:{[Ze<{A:B}>]:W}}"
      A.pType
      ( PB.NamedType
          ( astNode
              1
              44
              ( PB.AccessName
                  [ astNode 1 44 (PB.Name "Map")
                  ]
              )
          )
          [ astNode
              2
              26
              ( PB.NamedType
                  (astNode 2 10 (PB.AccessName [astNode 2 2 (PB.Name "io"), astNode 6 6 (PB.Name "Result")]))
                  [ astNode 13 6 (PB.NamedType (astNode 13 6 (PB.AccessName [astNode 13 1 (PB.Name "S"), astNode 16 3 (PB.Name "u32")])) []),
                    astNode
                      21
                      6
                      ( PB.NamedType
                          ( astNode
                              21
                              6
                              ( PB.AccessName
                                  [ astNode 21 1 (PB.Name "F"),
                                    astNode 24 3 (PB.Name "u64")
                                  ]
                              )
                          )
                          []
                      )
                  ]
              ),
            astNode
              29
              15
              ( PB.NamedType
                  (astNode 29 15 (PB.AccessName [astNode 29 15 (PB.Name "Map")]))
                  [ astNode
                      30
                      11
                      ( PB.NamedType
                          (astNode 30 11 (PB.AccessName [astNode 30 11 (PB.Name "List")]))
                          [ astNode
                              31
                              9
                              ( PB.NamedType
                                  (astNode 31 2 (PB.AccessName [astNode 31 2 (PB.Name "Ze")]))
                                  [ astNode
                                      34
                                      5
                                      ( PB.NamedType
                                          (astNode 34 5 (PB.AccessName [astNode 34 5 (PB.Name "Map")]))
                                          [ astNode 35 1 (PB.TypeVar (astNode 35 1 (PB.Name "A"))),
                                            astNode 37 1 (PB.TypeVar (astNode 37 1 (PB.Name "B")))
                                          ]
                                      )
                                  ]
                              )
                          ]
                      ),
                    astNode 42 1 (PB.TypeVar (astNode 42 1 (PB.Name "W")))
                  ]
              )
          ]
      ),
    -- function types parsing
    testAstOk "Test function type parsing 1" "(str) => u32" A.pType (PB.FunctionType [astNode 2 3 (PB.NamedType (astNode 2 3 (PB.AccessName [astNode 2 3 (PB.Name "str")])) [])] (astNode 10 3 (PB.NamedType (astNode 10 3 (PB.AccessName [astNode 10 3 (PB.Name "u32")])) []))),
    testAstOk "Test function type parsing 2" "(u64, u64) => math::Complex" A.pType (PB.FunctionType [astNode 2 3 (PB.NamedType (astNode 2 3 (PB.AccessName [astNode 2 3 (PB.Name "u64")])) []), astNode 7 3 (PB.NamedType (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "u64")])) [])] (astNode 15 13 (PB.NamedType (astNode 15 13 (PB.AccessName [astNode 15 4 (PB.Name "math"), astNode 21 7 (PB.Name "Complex")])) []))),
    testAstOk "Test function type parsing 3" "() => math::Complex" A.pType (PB.FunctionType [] (astNode 7 13 (PB.NamedType (astNode 7 13 (PB.AccessName [astNode 7 4 (PB.Name "math"), astNode 13 7 (PB.Name "Complex")])) []))),
    testAstOk "Test function type parsing 4" "(u64, (u64) => str) => B" A.pType (PB.FunctionType [astNode 2 3 (PB.NamedType (astNode 2 3 (PB.AccessName [astNode 2 3 (PB.Name "u64")])) []), astNode 7 12 (PB.FunctionType [astNode 8 3 (PB.NamedType (astNode 8 3 (PB.AccessName [astNode 8 3 (PB.Name "u64")])) [])] (astNode 16 3 (PB.NamedType (astNode 16 3 (PB.AccessName [astNode 16 3 (PB.Name "str")])) [])))] (astNode 24 1 (PB.TypeVar (astNode 24 1 (PB.Name "B"))))),
    testAstOk "Test function type parsing 2" "([T]) => {T}" A.pType (PB.FunctionType [astNode 2 3 (PB.NamedType (astNode 2 3 (PB.AccessName [astNode 2 3 (PB.Name "List")])) [astNode 3 1 (PB.TypeVar (astNode 3 1 (PB.Name "T")))])] (astNode 10 3 (PB.NamedType (astNode 10 3 (PB.AccessName [astNode 10 3 (PB.Name "Set")])) [astNode 11 1 (PB.TypeVar (astNode 11 1 (PB.Name "T")))])))
  ]

-- | Test trait statment parsing
traitTests :: [Test]
traitTests =
  [ testAstOk "test trait" "trait apply = <T> => (T) => u32" A.pTraitDef $ PB.Trait (astNode 7 6 (PB.Name "apply")) (astNode 15 4 (PB.ForAll [astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "T")))] [])) (astNode 22 10 (PB.FunctionType [astNode 23 1 (PB.TypeVar (astNode 23 1 (PB.Name "T")))] (astNode 29 3 (PB.NamedType (astNode 29 3 (PB.AccessName [astNode 29 3 (PB.Name "u32")])) [])))),
    testAstOk "test trait with bound" "trait apply = <T> where add<T> => (T) => u32" A.pTraitDef $ PB.Trait (astNode 7 6 (PB.Name "apply")) (astNode 15 17 (PB.ForAll [astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "T")))] [astNode 25 7 (PB.TraitBound (astNode 25 3 (PB.AccessName [astNode 25 3 (PB.Name "add")])) [astNode 29 1 (PB.TypeVar (astNode 29 1 (PB.Name "T")))])])) (astNode 35 10 (PB.FunctionType [astNode 36 1 (PB.TypeVar (astNode 36 1 (PB.Name "T")))] (astNode 42 3 (PB.NamedType (astNode 42 3 (PB.AccessName [astNode 42 3 (PB.Name "u32")])) [])))),
    testAstOk "test trait with bounds" "trait apply = <T> where add<T>, sub<T> => (T) => u32" A.pTraitDef $ PB.Trait (astNode 7 6 (PB.Name "apply")) (astNode 15 25 (PB.ForAll [astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "T")))] [astNode 25 6 (PB.TraitBound (astNode 25 3 (PB.AccessName [astNode 25 3 (PB.Name "add")])) [astNode 29 1 (PB.TypeVar (astNode 29 1 (PB.Name "T")))]), astNode 33 7 (PB.TraitBound (astNode 33 3 (PB.AccessName [astNode 33 3 (PB.Name "sub")])) [astNode 37 1 (PB.TypeVar (astNode 37 1 (PB.Name "T")))])])) (astNode 43 10 (PB.FunctionType [astNode 44 1 (PB.TypeVar (astNode 44 1 (PB.Name "T")))] (astNode 50 3 (PB.NamedType (astNode 50 3 (PB.AccessName [astNode 50 3 (PB.Name "u32")])) [])))),
    testAstFail "test trait without generics" "trait apply = (T) => u32" A.pTraitDef,
    testAstFail "test trait with no function type" "trait apply = <T> => T" A.pTraitDef,
    testAstFail "test trait with no function type or generics" "trait apply = u32" A.pTraitDef
  ]

-- | Test literal parsing
literalTests :: [Test]
literalTests =
  [ -- Chars

    testAstOk "Charater literal" "'a'" A.pLiteral (PB.CharLiteral 'a'),
    testAstFail "More than one char" "'ab'" A.pLiteral,
    testAstFail "Test un-closed char" "'a" A.pLiteral,
    testAstOk "Test control char" "'\\n'" A.pLiteral (PB.CharLiteral '\n'),
    testAstOk "Test apotsrophee char" "'\\''" A.pLiteral (PB.CharLiteral '\''),
    -- Strings

    testAstOk "Basic string" "\"a\"" A.pLiteral (PB.StrLiteral "a"),
    testAstFail "Unclosed string" "\"ab" A.pLiteral,
    testAstOk "string with control char in string" "\"ab\\ncd\"" A.pLiteral (PB.StrLiteral "ab\ncd"),
    testAstOk "string with utf chars" "\"ά\"" A.pLiteral (PB.StrLiteral "ά"),
    testAstOk "string with apotsrophee char" "\"\'\"" A.pLiteral (PB.StrLiteral "\'"),
    -- Integers

    testAstOk "Integer parsing" "2" A.pLiteral (PB.IntLiteral 2),
    testAstOk "Integer parsing" "21" A.pLiteral (PB.IntLiteral 21),
    testAstOk "Integer parsing" "002" A.pLiteral (PB.IntLiteral 2),
    testAstOk
      "Integer parsing"
      "2u32"
      A.pLiteral
      ( PB.IntLiteral
          2
      ),
    testAstOk
      "Integer parsing"
      "0230ibig"
      A.pLiteral
      ( PB.IntLiteral
          230
      ),
    -- Floats

    testAstOk "Float parsing 1 digit" "2.0" A.pLiteral (PB.FloatLiteral 2),
    testAstOk "Float parsing 2 digits" "21.0" A.pLiteral (PB.FloatLiteral 21),
    testAstOk "Float parsing with initial zeros" "002.0" A.pLiteral (PB.FloatLiteral 2),
    testAstOk
      "Float parsing with size type"
      "2.0f32"
      A.pLiteral
      ( PB.FloatLiteral
          2
      ),
    testAstOk
      "Float parsing with exponent"
      "2.0e3"
      A.pLiteral
      ( PB.FloatLiteral
          2000
      ),
    testAstOk
      "Float parsing with exponent and size type"
      "2.0e3f32"
      A.pLiteral
      ( PB.FloatLiteral
          2000
      ),
    -- List

    testAstOk "Empty list" "[]" A.pLiteral (PB.ListLiteral []),
    testAstFail "Empty list with trailing comma" "[,]" A.pLiteral,
    testAstOk
      "Single element list without trailling comma"
      "[1]"
      A.pLiteral
      ( PB.ListLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1)))
          ]
      ),
    testAstOk
      "Single element list with trailling comma"
      "[1,]"
      A.pLiteral
      ( PB.ListLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1)))
          ]
      ),
    testAstOk
      "Multiple element list with without trailling comma"
      "[1,2]"
      A.pLiteral
      ( PB.ListLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))),
            astNode 4 1 (PB.LiteralExpr (astNode 4 1 (PB.IntLiteral 2)))
          ]
      ),
    testAstOk
      "Empty list"
      "[1,2,]"
      A.pLiteral
      ( PB.ListLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))),
            astNode 4 1 (PB.LiteralExpr (astNode 4 1 (PB.IntLiteral 2)))
          ]
      ),
    -- Set

    testAstOk "Empty list" "{,}" A.pLiteral (PB.SetLiteral []),
    testAstFail "Empty tuple with trailing comma" "{}" A.pLiteral,
    testAstOk
      "Set with single element"
      "{1,}"
      A.pLiteral
      ( PB.SetLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1)))
          ]
      ),
    testAstFail "Set with single element without trailling comma" "{1}" A.pLiteral,
    testAstOk
      "Set with multiple elements, without trailling comma"
      "{1,2}"
      A.pLiteral
      ( PB.SetLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))),
            astNode 4 1 (PB.LiteralExpr (astNode 4 1 (PB.IntLiteral 2)))
          ]
      ),
    testAstOk
      "Set with multiple elements, with trailling comma"
      "{1,2,}"
      A.pLiteral
      ( PB.SetLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))),
            astNode 4 1 (PB.LiteralExpr (astNode 4 1 (PB.IntLiteral 2)))
          ]
      ),
    -- Tuples

    testAstOk "Empty tuple" "(,)" A.pLiteral (PB.TupleLiteral []),
    testAstFail "Empty tuple without comma" "()" A.pLiteral,
    testAstOk
      "Singular sized tuple"
      "(1,)"
      A.pLiteral
      ( PB.TupleLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1)))
          ]
      ),
    testAstFail "Singular tuple without ending comma" "(1)" A.pLiteral,
    testAstOk
      "2 elements in a tuple"
      "(1, 'a')"
      A.pLiteral
      ( PB.TupleLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))),
            astNode 5 3 (PB.LiteralExpr (astNode 5 3 (PB.CharLiteral 'a')))
          ]
      ),
    testAstOk
      "2 elements in a tuple with a trailing comma list"
      "(1, 'a',)"
      A.pLiteral
      ( PB.TupleLiteral
          [ astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))),
            astNode 5 3 (PB.LiteralExpr (astNode 5 3 (PB.CharLiteral 'a')))
          ]
      ),
    -- Maps

    testAstOk "Empty map" "{:}" A.pLiteral (PB.MapLiteral []),
    testAstFail "No colon for empty map" "{}" A.pLiteral,
    testAstFail "No key for map entry" "{:a}" A.pLiteral,
    testAstOk
      "Single map entry"
      "{1:2}"
      A.pLiteral
      ( PB.MapLiteral
          [ astNode
              2
              3
              ( PB.MapEntry
                  (astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))))
                  (astNode 4 1 (PB.LiteralExpr (astNode 4 1 (PB.IntLiteral 2))))
              )
          ]
      ),
    testAstOk
      "Single map entry with trailling comma"
      "{1:2,}"
      A.pLiteral
      ( PB.MapLiteral
          [ astNode
              2
              3
              ( PB.MapEntry
                  (astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))))
                  (astNode 4 1 (PB.LiteralExpr (astNode 4 1 (PB.IntLiteral 2))))
              )
          ]
      ),
    testAstOk
      "Map with 2 entries"
      "{1:2, 2:1}"
      A.pLiteral
      ( PB.MapLiteral
          [ astNode
              2
              3
              ( PB.MapEntry
                  (astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 1))))
                  (astNode 4 1 (PB.LiteralExpr (astNode 4 1 (PB.IntLiteral 2))))
              ),
            astNode
              7
              3
              ( PB.MapEntry
                  (astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 2))))
                  (astNode 9 1 (PB.LiteralExpr (astNode 9 1 (PB.IntLiteral 1))))
              )
          ]
      ),
    -- Struct literals
    testAstOk "Struct literal test 1" "Point {x=2; y=1}" A.pCompoundLiteral (PB.StructLiteral (astNode 1 6 (PB.NamedType (astNode 1 6 (PB.AccessName [astNode 1 6 (PB.Name "Point")])) [])) (HM.fromList [(astNode 13 1 (PB.Name "y"), astNode 15 1 (PB.LiteralExpr (astNode 15 1 (PB.IntLiteral 1)))), (astNode 8 1 (PB.Name "x"), astNode 10 1 (PB.LiteralExpr (astNode 10 1 (PB.IntLiteral 2))))])),
    testAstOk "Struct literal test 2" "Point {x=2; y=1;}" A.pCompoundLiteral (PB.StructLiteral (astNode 1 6 (PB.NamedType (astNode 1 6 (PB.AccessName [astNode 1 6 (PB.Name "Point")])) [])) (HM.fromList [(astNode 13 1 (PB.Name "y"), astNode 15 1 (PB.LiteralExpr (astNode 15 1 (PB.IntLiteral 1)))), (astNode 8 1 (PB.Name "x"), astNode 10 1 (PB.LiteralExpr (astNode 10 1 (PB.IntLiteral 2))))])),
    testAstOk "Struct literal test 3" "Rect {start=Point {x=2; y=1}; end=Point {x=2; y=1}}" A.pCompoundLiteral (PB.StructLiteral (astNode 1 5 (PB.NamedType (astNode 1 5 (PB.AccessName [astNode 1 5 (PB.Name "Rect")])) [])) (HM.fromList [(astNode 31 3 (PB.Name "end"), astNode 35 16 (PB.LiteralExpr (astNode 35 16 (PB.StructLiteral (astNode 35 6 (PB.NamedType (astNode 35 6 (PB.AccessName [astNode 35 6 (PB.Name "Point")])) [])) (HM.fromList [(astNode 47 1 (PB.Name "y"), astNode 49 1 (PB.LiteralExpr (astNode 49 1 (PB.IntLiteral 1)))), (astNode 42 1 (PB.Name "x"), astNode 44 1 (PB.LiteralExpr (astNode 44 1 (PB.IntLiteral 2))))]))))), (astNode 7 5 (PB.Name "start"), astNode 13 16 (PB.LiteralExpr (astNode 13 16 (PB.StructLiteral (astNode 13 6 (PB.NamedType (astNode 13 6 (PB.AccessName [astNode 13 6 (PB.Name "Point")])) [])) (HM.fromList [(astNode 20 1 (PB.Name "x"), astNode 22 1 (PB.LiteralExpr (astNode 22 1 (PB.IntLiteral 2)))), (astNode 25 1 (PB.Name "y"), astNode 27 1 (PB.LiteralExpr (astNode 27 1 (PB.IntLiteral 1))))])))))])),
    testAstOk "Struct literal test 4" "Empty {}" A.pCompoundLiteral (PB.StructLiteral (astNode 1 6 (PB.NamedType (astNode 1 6 (PB.AccessName [astNode 1 6 (PB.Name "Empty")])) [])) (HM.fromList [])),
    testAstOk "Struct literal test 5" "math::Complex {a=3; b=-2}" A.pCompoundLiteral (PB.StructLiteral (astNode 1 14 (PB.NamedType (astNode 1 14 (PB.AccessName [astNode 1 4 (PB.Name "math"), astNode 7 8 (PB.Name "Complex")])) [])) (HM.fromList [(astNode 21 1 (PB.Name "b"), astNode 23 2 (PB.FunctionCall (astNode 23 1 (PB.Variable (astNode 23 1 (PB.AccessName [astNode 23 1 (PB.Name "neg")])))) (astNode 24 1 (funcCallArgs Nothing [astNode 24 1 (PB.LiteralExpr (astNode 24 1 (PB.IntLiteral 2)))])))), (astNode 16 1 (PB.Name "a"), astNode 18 1 (PB.LiteralExpr (astNode 18 1 (PB.IntLiteral 3))))])),
    testAstFail "Sturct literal test 6" "Point {;}" A.pCompoundLiteral,
    testAstFail "Struct literal test 7" "Point {x;y}" A.pCompoundLiteral, -- although that would be cool!

    -- Function literals
    testAstOk "Function literal test 1" "(a, b) => a + b;" A.pCompoundLiteral (PB.FunctionLiteral [astNode 2 1 (PB.FunctionParam (astNode 2 1 (PB.Name "a")) Nothing), astNode 5 1 (PB.FunctionParam (astNode 5 1 (PB.Name "b")) Nothing)] Nothing (astNode 11 5 (PB.FunctionCall (astNode 13 2 (PB.Variable (astNode 13 2 (PB.AccessName [astNode 13 2 (PB.Name "add")])))) (astNode 11 5 (funcCallArgs Nothing [astNode 11 2 (PB.Variable (astNode 11 2 (PB.AccessName [astNode 11 2 (PB.Name "a")]))), astNode 15 1 (PB.Variable (astNode 15 1 (PB.AccessName [astNode 15 1 (PB.Name "b")])))]))))),
    testAstOk "Function literal test 2" "(a: str, b) => a + b;" A.pCompoundLiteral (PB.FunctionLiteral [astNode 2 6 (PB.FunctionParam (astNode 2 1 (PB.Name "a")) (Just (astNode 5 3 (PB.NamedType (astNode 5 3 (PB.AccessName [astNode 5 3 (PB.Name "str")])) [])))), astNode 10 1 (PB.FunctionParam (astNode 10 1 (PB.Name "b")) Nothing)] Nothing (astNode 16 5 (PB.FunctionCall (astNode 18 2 (PB.Variable (astNode 18 2 (PB.AccessName [astNode 18 2 (PB.Name "add")])))) (astNode 16 5 (funcCallArgs Nothing [astNode 16 2 (PB.Variable (astNode 16 2 (PB.AccessName [astNode 16 2 (PB.Name "a")]))), astNode 20 1 (PB.Variable (astNode 20 1 (PB.AccessName [astNode 20 1 (PB.Name "b")])))]))))),
    testAstOk "Function literal test 3" "(a) => print(a);" A.pCompoundLiteral (PB.FunctionLiteral [astNode 2 1 (PB.FunctionParam (astNode 2 1 (PB.Name "a")) Nothing)] Nothing (astNode 8 8 (PB.FunctionCall (astNode 8 5 (PB.Variable (astNode 8 5 (PB.AccessName [astNode 8 5 (PB.Name "print")])))) (astNode 13 3 (funcCallArgs Nothing [astNode 14 1 (PB.Variable (astNode 14 1 (PB.AccessName [astNode 14 1 (PB.Name "a")])))]))))),
    testAstFail "Function literal test 5" "(a)" A.pFunctionLiteral,
    testAstFail "Function literal test 6" "(a,)" A.pFunctionLiteral,
    testAstFail "Function literal test 7" "() =>;" A.pFunctionLiteral,
    testAstFail "Function literal test 8" "(a): 2" A.pFunctionLiteral,
    testAstFail "Function definitions with multiple parameters should not have a trailing comma" "(a: str, b: str,) => a == b;" A.pFunctionLiteral,
    testAstFail "Function definitions should have an arrow between parameters and an expression" "(a: str, b: str) = a == b;" A.pFunctionLiteral,
    testAstFail "Function definitions should have an expression to execute" "(a: str, b: str) =>;" A.pFunctionLiteral,
    testAstOk "Function literal test 10" "() => {}" A.pCompoundLiteral (PB.FunctionLiteral [] Nothing (astNode 7 2 (PB.BlockExpr (astNode 7 2 (PB.Body [] Nothing))))),
    testAstFail "Function literal test 10" "(,) => {}" A.pFunctionLiteral,
    testAstOk "Function literal test 11" "(a: T, b: (T) => E): [E] => b(a)" A.pCompoundLiteral (PB.FunctionLiteral [astNode 2 4 (PB.FunctionParam (astNode 2 1 (PB.Name "a")) (Just (astNode 5 1 (PB.TypeVar (astNode 5 1 (PB.Name "T")))))), astNode 8 11 (PB.FunctionParam (astNode 8 1 (PB.Name "b")) (Just (astNode 11 8 (PB.FunctionType [astNode 12 1 (PB.TypeVar (astNode 12 1 (PB.Name "T")))] (astNode 18 1 (PB.TypeVar (astNode 18 1 (PB.Name "E"))))))))] (Just (astNode 22 4 (PB.NamedType (astNode 22 4 (PB.AccessName [astNode 22 4 (PB.Name "List")])) [astNode 23 1 (PB.TypeVar (astNode 23 1 (PB.Name "E")))]))) (astNode 29 4 (PB.FunctionCall (astNode 29 1 (PB.Variable (astNode 29 1 (PB.AccessName [astNode 29 1 (PB.Name "b")])))) (astNode 30 3 (funcCallArgs Nothing [astNode 31 1 (PB.Variable (astNode 31 1 (PB.AccessName [astNode 31 1 (PB.Name "a")])))])))))
  ]

-- | Test Enum definition parsing
enumDefTests :: [Test]
enumDefTests =
  [ testAstOk "Empty enum definition" "enum Empty = {};" A.pEnum (PB.Enumeration (astNode 6 6 (PB.Name "Empty")) (astNode 14 0 (PB.ForAll [] [])) []),
    testAstFail "Enum definition without body" "enum Empty;" A.pEnum,
    testAstOk "Enum definition with members" "enum AnEnum = {Field;};" A.pEnum (PB.Enumeration (astNode 6 7 (PB.Name "AnEnum")) (astNode 15 0 (PB.ForAll [] [])) [astNode 16 6 (PB.EnumEntry (astNode 16 5 (PB.Name "Field")) [])]),
    testAstFail "Enum with member that is unclosed by a semicolon" "enum AnEnum = {Field};" A.pEnum,
    testAstOk "Enum with multiple members" "enum AnEnum = {Field;SecondField;};" A.pEnum (PB.Enumeration (astNode 6 7 (PB.Name "AnEnum")) (astNode 15 0 (PB.ForAll [] [])) [astNode 16 6 (PB.EnumEntry (astNode 16 5 (PB.Name "Field")) []), astNode 22 12 (PB.EnumEntry (astNode 22 11 (PB.Name "SecondField")) [])]),
    testAstOk "Enum with type annotation" "enum AnEnum = <T> => {Field;};" A.pEnum (PB.Enumeration (astNode 6 7 (PB.Name "AnEnum")) (astNode 15 4 (PB.ForAll [astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "T")))] [])) [astNode 23 6 (PB.EnumEntry (astNode 23 5 (PB.Name "Field")) [])]),
    testAstFail "Enum type annotations must be followed by an arrow" "enum AnEnum = <T> {Field;};" A.pEnum,
    testAstFail "Enum type annotations must have at least one type" "enum AnEnum = <> => {Field;};" A.pEnum,
    testAstOk "Enum with multiple type annotations" "enum AnEnum = <T, E> => {Field;};" A.pEnum (PB.Enumeration (astNode 6 7 (PB.Name "AnEnum")) (astNode 15 7 (PB.ForAll [astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "T"))), astNode 19 1 (PB.TypeVar (astNode 19 1 (PB.Name "E")))] [])) [astNode 26 6 (PB.EnumEntry (astNode 26 5 (PB.Name "Field")) [])]),
    testAstFail "Enum with multiple type annotations should not have a trailling comma" "enum AnEnum = <T, E, > => {Field;};" A.pEnum,
    testAstOk "Enum members referencing types" "enum AnEnum = <T, E> => {Field(T);};" A.pEnum (PB.Enumeration (astNode 6 7 (PB.Name "AnEnum")) (astNode 15 7 (PB.ForAll [astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "T"))), astNode 19 1 (PB.TypeVar (astNode 19 1 (PB.Name "E")))] [])) [astNode 26 9 (PB.EnumEntry (astNode 26 5 (PB.Name "Field")) [astNode 32 1 (PB.TypeVar (astNode 32 1 (PB.Name "T")))])]),
    testAstFail "Enum members referencing types should not have trailling commas" "enum AnEnum = <T, E> => {Field(T,);};" A.pEnum,
    testAstFail "Enum members referencing types must reference at least one type" "enum AnEnum = <T, E> => {Field();};" A.pEnum,
    testAstOk "Enum members referencing multiple types" "enum AnEnum = <T, E> => {Field(T, E);};" A.pEnum (PB.Enumeration (astNode 6 7 (PB.Name "AnEnum")) (astNode 15 7 (PB.ForAll [astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "T"))), astNode 19 1 (PB.TypeVar (astNode 19 1 (PB.Name "E")))] [])) [astNode 26 12 (PB.EnumEntry (astNode 26 5 (PB.Name "Field")) [astNode 32 1 (PB.TypeVar (astNode 32 1 (PB.Name "T"))), astNode 35 1 (PB.TypeVar (astNode 35 1 (PB.Name "E")))])])
  ]

-- | Test enum statement parsing
structDefTests :: [Test]
structDefTests =
  [ testAstOk "Empty struct definition" "struct Empty = {};" A.pStruct (PB.Struct (astNode 8 6 (PB.Name "Empty")) (astNode 16 0 (PB.ForAll [] [])) []),
    testAstFail "Struct definition without body" "struct Empty;" A.pStruct,
    testAstOk "Struct definition with single field" "struct t = {field;};" A.pStruct (PB.Struct (astNode 8 2 (PB.Name "t")) (astNode 12 0 (PB.ForAll [] [])) [astNode 13 6 (PB.StructEntry (astNode 13 5 (PB.Name "field")) Nothing Nothing)]),
    testAstOk "Struct definition with single typed field" "struct t = {field: u32;};" A.pStruct (PB.Struct (astNode 8 2 (PB.Name "t")) (astNode 12 0 (PB.ForAll [] [])) [astNode 13 11 (PB.StructEntry (astNode 13 5 (PB.Name "field")) (Just (astNode 20 3 (PB.NamedType (astNode 20 3 (PB.AccessName [astNode 20 3 (PB.Name "u32")])) []))) Nothing)]),
    testAstFail "Struct field that is unclosed by a semicolon" "struct t = {field: u32};" A.pStruct,
    testAstOk "Struct with multiple members" "struct t = {field; second_field;};" A.pStruct (PB.Struct (astNode 8 2 (PB.Name "t")) (astNode 12 0 (PB.ForAll [] [])) [astNode 13 7 (PB.StructEntry (astNode 13 5 (PB.Name "field")) Nothing Nothing), astNode 20 13 (PB.StructEntry (astNode 20 12 (PB.Name "second_field")) Nothing Nothing)]),
    testAstOk "Struct with type annotation" "struct t = <T> => {field;};" A.pStruct (PB.Struct (astNode 8 2 (PB.Name "t")) (astNode 12 4 (PB.ForAll [astNode 13 1 (PB.TypeVar (astNode 13 1 (PB.Name "T")))] [])) [astNode 20 6 (PB.StructEntry (astNode 20 5 (PB.Name "field")) Nothing Nothing)]),
    testAstFail "Struct type annotations must be followed by an arrow" "struct t = <T> {field;};" A.pStruct,
    testAstFail "Struct type annotations must have at least one type" "struct t = <> => {field;};" A.pStruct,
    testAstOk "Struct with multiple type annotations" "struct t = <T, E> => {field;};" A.pStruct (PB.Struct (astNode 8 2 (PB.Name "t")) (astNode 12 7 (PB.ForAll [astNode 13 1 (PB.TypeVar (astNode 13 1 (PB.Name "T"))), astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "E")))] [])) [astNode 23 6 (PB.StructEntry (astNode 23 5 (PB.Name "field")) Nothing Nothing)]),
    testAstFail "Struct with multiple type annotations should not have a trailling comma" "struct t = <T, E, > => {field;};" A.pStruct,
    testAstOk "Struct members referencing types" "struct t = <T, E> => {x: T;};" A.pStruct (PB.Struct (astNode 8 2 (PB.Name "t")) (astNode 12 7 (PB.ForAll [astNode 13 1 (PB.TypeVar (astNode 13 1 (PB.Name "T"))), astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "E")))] [])) [astNode 23 5 (PB.StructEntry (astNode 23 1 (PB.Name "x")) (Just (astNode 26 1 (PB.TypeVar (astNode 26 1 (PB.Name "T"))))) Nothing)]),
    testAstOk "Struct fields referencing different types" "struct t = <T, E> => {x: T; y: E; z: str;};" A.pStruct (PB.Struct (astNode 8 2 (PB.Name "t")) (astNode 12 7 (PB.ForAll [astNode 13 1 (PB.TypeVar (astNode 13 1 (PB.Name "T"))), astNode 16 1 (PB.TypeVar (astNode 16 1 (PB.Name "E")))] [])) [astNode 23 6 (PB.StructEntry (astNode 23 1 (PB.Name "x")) (Just (astNode 26 1 (PB.TypeVar (astNode 26 1 (PB.Name "T"))))) Nothing), astNode 29 6 (PB.StructEntry (astNode 29 1 (PB.Name "y")) (Just (astNode 32 1 (PB.TypeVar (astNode 32 1 (PB.Name "E"))))) Nothing), astNode 35 7 (PB.StructEntry (astNode 35 1 (PB.Name "z")) (Just (astNode 38 3 (PB.NamedType (astNode 38 3 (PB.AccessName [astNode 38 3 (PB.Name "str")])) []))) Nothing)])
  ]

-- | Tests for the 'if-else' expressions, since they are transpilled into
ifElseTests :: [Test]
ifElseTests =
  [ testAstOk "Parse basic if with empty block" "if x {}" A.pIf (PB.Match (astNode 8 0 (PB.Variable (astNode 8 0 (PB.AccessName [astNode 8 0 (PB.Name "true")])))) [astNode 1 7 (PB.MatchCase (astNode 4 2 (PB.PatternIf (astNode 4 2 PB.PatternIgnore) (astNode 4 2 (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "x")])))))) (astNode 6 2 (PB.BlockExpr (astNode 6 2 (PB.Body [] Nothing))))), astNode 8 0 (PB.MatchCase (astNode 8 0 PB.PatternIgnore) (astNode 8 0 (PB.BlockExpr (astNode 8 0 (PB.Body [] Nothing)))))]),
    testAstOk "Parse basic if with block" "if x {2}" A.pIf (PB.Match (astNode 9 0 (PB.Variable (astNode 9 0 (PB.AccessName [astNode 9 0 (PB.Name "true")])))) [astNode 1 8 (PB.MatchCase (astNode 4 2 (PB.PatternIf (astNode 4 2 PB.PatternIgnore) (astNode 4 2 (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "x")])))))) (astNode 6 3 (PB.BlockExpr (astNode 6 3 (PB.Body [] (Just (astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 2)))))))))), astNode 9 0 (PB.MatchCase (astNode 9 0 PB.PatternIgnore) (astNode 9 0 (PB.BlockExpr (astNode 9 0 (PB.Body [] Nothing)))))]),
    testAstOk "Parse basic with condition block" "if {x} {2}" A.pIf (PB.Match (astNode 11 0 (PB.Variable (astNode 11 0 (PB.AccessName [astNode 11 0 (PB.Name "true")])))) [astNode 1 10 (PB.MatchCase (astNode 4 4 (PB.PatternIf (astNode 4 4 PB.PatternIgnore) (astNode 4 4 (PB.BlockExpr (astNode 4 4 (PB.Body [] (Just (astNode 5 1 (PB.Variable (astNode 5 1 (PB.AccessName [astNode 5 1 (PB.Name "x")]))))))))))) (astNode 8 3 (PB.BlockExpr (astNode 8 3 (PB.Body [] (Just (astNode 9 1 (PB.LiteralExpr (astNode 9 1 (PB.IntLiteral 2)))))))))), astNode 11 0 (PB.MatchCase (astNode 11 0 PB.PatternIgnore) (astNode 11 0 (PB.BlockExpr (astNode 11 0 (PB.Body [] Nothing)))))]),
    testAstOk "Parse basic if with optional block termination" "if {x;} {2}" A.pIf (PB.Match (astNode 12 0 (PB.Variable (astNode 12 0 (PB.AccessName [astNode 12 0 (PB.Name "true")])))) [astNode 1 11 (PB.MatchCase (astNode 4 5 (PB.PatternIf (astNode 4 5 PB.PatternIgnore) (astNode 4 5 (PB.BlockExpr (astNode 4 5 (PB.Body [astNode 5 1 (PB.ExprStatement (astNode 5 1 (PB.Variable (astNode 5 1 (PB.AccessName [astNode 5 1 (PB.Name "x")])))))] Nothing)))))) (astNode 9 3 (PB.BlockExpr (astNode 9 3 (PB.Body [] (Just (astNode 10 1 (PB.LiteralExpr (astNode 10 1 (PB.IntLiteral 2)))))))))), astNode 12 0 (PB.MatchCase (astNode 12 0 PB.PatternIgnore) (astNode 12 0 (PB.BlockExpr (astNode 12 0 (PB.Body [] Nothing)))))]),
    testAstOk "Parse basic if with multiple block statements" "if {do(x); x == 3} {2}" A.pIf (PB.Match (astNode 23 0 (PB.Variable (astNode 23 0 (PB.AccessName [astNode 23 0 (PB.Name "true")])))) [astNode 1 22 (PB.MatchCase (astNode 4 16 (PB.PatternIf (astNode 4 16 PB.PatternIgnore) (astNode 4 16 (PB.BlockExpr (astNode 4 16 (PB.Body [astNode 5 5 (PB.ExprStatement (astNode 5 5 (PB.FunctionCall (astNode 5 2 (PB.Variable (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "do")])))) (astNode 7 3 (funcCallArgs Nothing [astNode 8 1 (PB.Variable (astNode 8 1 (PB.AccessName [astNode 8 1 (PB.Name "x")])))])))))] (Just (astNode 12 6 (PB.FunctionCall (astNode 14 3 (PB.Variable (astNode 14 3 (PB.AccessName [astNode 14 3 (PB.Name "eq")])))) (astNode 12 6 (funcCallArgs Nothing [astNode 12 2 (PB.Variable (astNode 12 2 (PB.AccessName [astNode 12 2 (PB.Name "x")]))), astNode 17 1 (PB.LiteralExpr (astNode 17 1 (PB.IntLiteral 3)))]))))))))))) (astNode 20 3 (PB.BlockExpr (astNode 20 3 (PB.Body [] (Just (astNode 21 1 (PB.LiteralExpr (astNode 21 1 (PB.IntLiteral 2)))))))))), astNode 23 0 (PB.MatchCase (astNode 23 0 PB.PatternIgnore) (astNode 23 0 (PB.BlockExpr (astNode 23 0 (PB.Body [] Nothing)))))]),
    testAstFail "Parse if with unexpected termination" "if x; {2}" A.pIf,
    testAstFail "Parse if with unclosed condition" "if {x; {2}" A.pIf,
    testAstFail "Parse if with improper block" "if {x} 2" A.pIf,
    testAstFail "Parse if without statment" "if x" A.pIf,
    testAstOk "Parse basic if with specified else" "if x {2} else {3}" A.pIf (PB.Match (astNode 18 0 (PB.Variable (astNode 18 0 (PB.AccessName [astNode 18 0 (PB.Name "true")])))) [astNode 1 9 (PB.MatchCase (astNode 4 2 (PB.PatternIf (astNode 4 2 PB.PatternIgnore) (astNode 4 2 (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "x")])))))) (astNode 6 4 (PB.BlockExpr (astNode 6 4 (PB.Body [] (Just (astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 2)))))))))), astNode 10 8 (PB.MatchCase (astNode 10 0 PB.PatternIgnore) (astNode 15 3 (PB.BlockExpr (astNode 15 3 (PB.Body [] (Just (astNode 16 1 (PB.LiteralExpr (astNode 16 1 (PB.IntLiteral 3))))))))))]),
    testAstFail "Parse if with specified else lacking block" "if {x} {2} else" A.pIf,
    testAstOk "Parse if with 'else-if' clause" "if x {2} else if y {3}" A.pIf (PB.Match (astNode 23 0 (PB.Variable (astNode 23 0 (PB.AccessName [astNode 23 0 (PB.Name "true")])))) [astNode 1 9 (PB.MatchCase (astNode 4 2 (PB.PatternIf (astNode 4 2 PB.PatternIgnore) (astNode 4 2 (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "x")])))))) (astNode 6 4 (PB.BlockExpr (astNode 6 4 (PB.Body [] (Just (astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 2)))))))))), astNode 15 8 (PB.MatchCase (astNode 18 2 (PB.PatternIf (astNode 18 2 PB.PatternIgnore) (astNode 18 2 (PB.Variable (astNode 18 2 (PB.AccessName [astNode 18 2 (PB.Name "y")])))))) (astNode 20 3 (PB.BlockExpr (astNode 20 3 (PB.Body [] (Just (astNode 21 1 (PB.LiteralExpr (astNode 21 1 (PB.IntLiteral 3)))))))))), astNode 23 0 (PB.MatchCase (astNode 23 0 PB.PatternIgnore) (astNode 23 0 (PB.BlockExpr (astNode 23 0 (PB.Body [] Nothing)))))]),
    testAstOk "Parse if with 'else-if' and 'else' clause" "if x {2}  else if y {3} else {4}" A.pIf (PB.Match (astNode 33 0 (PB.Variable (astNode 33 0 (PB.AccessName [astNode 33 0 (PB.Name "true")])))) [astNode 1 10 (PB.MatchCase (astNode 4 2 (PB.PatternIf (astNode 4 2 PB.PatternIgnore) (astNode 4 2 (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "x")])))))) (astNode 6 5 (PB.BlockExpr (astNode 6 5 (PB.Body [] (Just (astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 2)))))))))), astNode 16 9 (PB.MatchCase (astNode 19 2 (PB.PatternIf (astNode 19 2 PB.PatternIgnore) (astNode 19 2 (PB.Variable (astNode 19 2 (PB.AccessName [astNode 19 2 (PB.Name "y")])))))) (astNode 21 4 (PB.BlockExpr (astNode 21 4 (PB.Body [] (Just (astNode 22 1 (PB.LiteralExpr (astNode 22 1 (PB.IntLiteral 3)))))))))), astNode 25 8 (PB.MatchCase (astNode 25 0 PB.PatternIgnore) (astNode 30 3 (PB.BlockExpr (astNode 30 3 (PB.Body [] (Just (astNode 31 1 (PB.LiteralExpr (astNode 31 1 (PB.IntLiteral 4))))))))))]),
    testAstFail "Parse if with invalid else-if statement" "if x {2} if y {3}" (A.pIf <* M.eof),
    testAstFail "Parse if with improper keyword order" "if x {2} if else y {}" (A.pIf <* M.eof),
    testAstOk "Parse if with 'else-if' clause lacking block" "if x {2} else if y {}" A.pIf (PB.Match (astNode 22 0 (PB.Variable (astNode 22 0 (PB.AccessName [astNode 22 0 (PB.Name "true")])))) [astNode 1 9 (PB.MatchCase (astNode 4 2 (PB.PatternIf (astNode 4 2 PB.PatternIgnore) (astNode 4 2 (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "x")])))))) (astNode 6 4 (PB.BlockExpr (astNode 6 4 (PB.Body [] (Just (astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 2)))))))))), astNode 15 7 (PB.MatchCase (astNode 18 2 (PB.PatternIf (astNode 18 2 PB.PatternIgnore) (astNode 18 2 (PB.Variable (astNode 18 2 (PB.AccessName [astNode 18 2 (PB.Name "y")])))))) (astNode 20 2 (PB.BlockExpr (astNode 20 2 (PB.Body [] Nothing))))), astNode 22 0 (PB.MatchCase (astNode 22 0 PB.PatternIgnore) (astNode 22 0 (PB.BlockExpr (astNode 22 0 (PB.Body [] Nothing)))))])
  ]

-- | Test loop parsing
loopWhileForTests :: [Test]
loopWhileForTests =
  [ -- loop
    testAstOk "Parse loop (1): simple loop" "loop {}" A.pLoop (PB.Loop (astNode 6 2 (PB.Body [] Nothing))),
    testAstFail "Parse loop (2): non-block statement" "loop func()" A.pLoop,
    testAstOk "Parse loop (2): non-block statement" "loop {func()}" A.pLoop (PB.Loop (astNode 6 8 (PB.Body [] (Just (astNode 7 6 (PB.FunctionCall (astNode 7 4 (PB.Variable (astNode 7 4 (PB.AccessName [astNode 7 4 (PB.Name "func")])))) (astNode 11 2 (funcCallArgs Nothing [])))))))),
    -- for
    testAstOk "Parse simple for statement" "for x in xs {}" A.pFor (PB.Loop (astNode 1 14 (PB.Match (astNode 10 3 (PB.FunctionCall (astNode 10 3 (PB.Variable (astNode 10 3 (PB.AccessName [astNode 10 3 (PB.Name "next")])))) (astNode 10 3 (funcCallArgs Nothing [astNode 10 3 (PB.Variable (astNode 10 3 (PB.AccessName [astNode 10 3 (PB.Name "xs")])))])))) [astNode 1 14 (PB.MatchCase (astNode 5 2 (PB.PatternEnum (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "Some")])) [astNode 5 2 (PB.PatternBinding (astNode 5 2 (PB.Name "x")))])) (astNode 13 2 (PB.BlockExpr (astNode 13 2 (PB.Body [] Nothing))))), astNode 1 14 (PB.MatchCase (astNode 5 2 (PB.PatternEnum (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "None")])) [])) (astNode 1 14 (PB.BlockExpr (astNode 1 14 (PB.Body [astNode 1 14 PB.Break] Nothing)))))]))),
    testAstOk "Parse for statement with destructing" "for (x,y) in tups {}" A.pFor (PB.Loop (astNode 1 20 (PB.Match (astNode 14 5 (PB.FunctionCall (astNode 14 5 (PB.Variable (astNode 14 5 (PB.AccessName [astNode 14 5 (PB.Name "next")])))) (astNode 14 5 (funcCallArgs Nothing [astNode 14 5 (PB.Variable (astNode 14 5 (PB.AccessName [astNode 14 5 (PB.Name "tups")])))])))) [astNode 1 20 (PB.MatchCase (astNode 5 6 (PB.PatternEnum (astNode 5 6 (PB.AccessName [astNode 5 6 (PB.Name "Some")])) [astNode 5 6 (PB.PatternTuple [astNode 6 1 (PB.PatternBinding (astNode 6 1 (PB.Name "x"))), astNode 8 1 (PB.PatternBinding (astNode 8 1 (PB.Name "y")))])])) (astNode 19 2 (PB.BlockExpr (astNode 19 2 (PB.Body [] Nothing))))), astNode 1 20 (PB.MatchCase (astNode 5 6 (PB.PatternEnum (astNode 5 6 (PB.AccessName [astNode 5 6 (PB.Name "None")])) [])) (astNode 1 20 (PB.BlockExpr (astNode 1 20 (PB.Body [astNode 1 20 PB.Break] Nothing)))))]))),
    testAstOk "Parse for statment with ignore pattern" "for _ in xs {}" A.pFor (PB.Loop (astNode 1 14 (PB.Match (astNode 10 3 (PB.FunctionCall (astNode 10 3 (PB.Variable (astNode 10 3 (PB.AccessName [astNode 10 3 (PB.Name "next")])))) (astNode 10 3 (funcCallArgs Nothing [astNode 10 3 (PB.Variable (astNode 10 3 (PB.AccessName [astNode 10 3 (PB.Name "xs")])))])))) [astNode 1 14 (PB.MatchCase (astNode 5 2 (PB.PatternEnum (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "Some")])) [astNode 5 2 PB.PatternIgnore])) (astNode 13 2 (PB.BlockExpr (astNode 13 2 (PB.Body [] Nothing))))), astNode 1 14 (PB.MatchCase (astNode 5 2 (PB.PatternEnum (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "None")])) [])) (astNode 1 14 (PB.BlockExpr (astNode 1 14 (PB.Body [astNode 1 14 PB.Break] Nothing)))))]))),
    testAstFail "Parse for with no destructing pattern" "for in xs {}" A.pFor,
    testAstFail "Parse for with no generator statement 1" "for x in {}" A.pFor,
    testAstOk "Parse for with no generator statement 2" "for x in {} {}" A.pFor (PB.Loop (astNode 1 14 (PB.Match (astNode 10 3 (PB.FunctionCall (astNode 10 3 (PB.Variable (astNode 10 3 (PB.AccessName [astNode 10 3 (PB.Name "next")])))) (astNode 10 3 (funcCallArgs Nothing [astNode 10 3 (PB.BlockExpr (astNode 10 3 (PB.Body [] Nothing)))])))) [astNode 1 14 (PB.MatchCase (astNode 5 2 (PB.PatternEnum (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "Some")])) [astNode 5 2 (PB.PatternBinding (astNode 5 2 (PB.Name "x")))])) (astNode 13 2 (PB.BlockExpr (astNode 13 2 (PB.Body [] Nothing))))), astNode 1 14 (PB.MatchCase (astNode 5 2 (PB.PatternEnum (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "None")])) [])) (astNode 1 14 (PB.BlockExpr (astNode 1 14 (PB.Body [astNode 1 14 PB.Break] Nothing)))))]))),
    testAstFail "Parse for with no generator statement 3" "for x in { {}" A.pFor,
    testAstOk "Parse for with literal generator 1" "for x in [1,2,3,4].iter() {}" A.pFor (PB.Loop (astNode 1 28 (PB.Match (astNode 10 17 (PB.FunctionCall (astNode 10 17 (PB.Variable (astNode 10 17 (PB.AccessName [astNode 10 17 (PB.Name "next")])))) (astNode 10 17 (funcCallArgs Nothing [astNode 10 17 (PB.FunctionCall (astNode 20 4 (PB.Variable (astNode 20 4 (PB.AccessName [astNode 20 4 (PB.Name "iter")])))) (astNode 10 17 (funcCallArgs Nothing [astNode 10 9 (PB.LiteralExpr (astNode 10 9 (PB.ListLiteral [astNode 11 1 (PB.LiteralExpr (astNode 11 1 (PB.IntLiteral 1))), astNode 13 1 (PB.LiteralExpr (astNode 13 1 (PB.IntLiteral 2))), astNode 15 1 (PB.LiteralExpr (astNode 15 1 (PB.IntLiteral 3))), astNode 17 1 (PB.LiteralExpr (astNode 17 1 (PB.IntLiteral 4)))])))])))])))) [astNode 1 28 (PB.MatchCase (astNode 5 2 (PB.PatternEnum (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "Some")])) [astNode 5 2 (PB.PatternBinding (astNode 5 2 (PB.Name "x")))])) (astNode 27 2 (PB.BlockExpr (astNode 27 2 (PB.Body [] Nothing))))), astNode 1 28 (PB.MatchCase (astNode 5 2 (PB.PatternEnum (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "None")])) [])) (astNode 1 28 (PB.BlockExpr (astNode 1 28 (PB.Body [astNode 1 28 PB.Break] Nothing)))))]))),
    -- while
    testAstOk "Parse while (1): statement" "while x {}" A.pWhile (PB.Loop (astNode 1 10 (PB.Match (astNode 7 2 (PB.Variable (astNode 7 2 (PB.AccessName [astNode 7 2 (PB.Name "x")])))) [astNode 1 10 (PB.MatchCase (astNode 7 2 (PB.PatternEnum (astNode 7 2 (PB.AccessName [astNode 7 2 (PB.Name "true")])) [])) (astNode 9 2 (PB.BlockExpr (astNode 9 2 (PB.Body [] Nothing))))), astNode 1 10 (PB.MatchCase (astNode 7 2 (PB.PatternEnum (astNode 7 2 (PB.AccessName [astNode 7 2 (PB.Name "false")])) [])) (astNode 1 10 (PB.BlockExpr (astNode 1 10 (PB.Body [astNode 1 10 PB.Break] Nothing)))))]))),
    testAstOk "Parse while (2): statement with block condition" "while {x} {}" A.pWhile (PB.Loop (astNode 1 12 (PB.Match (astNode 7 4 (PB.BlockExpr (astNode 7 4 (PB.Body [] (Just (astNode 8 1 (PB.Variable (astNode 8 1 (PB.AccessName [astNode 8 1 (PB.Name "x")]))))))))) [astNode 1 12 (PB.MatchCase (astNode 7 4 (PB.PatternEnum (astNode 7 4 (PB.AccessName [astNode 7 4 (PB.Name "true")])) [])) (astNode 11 2 (PB.BlockExpr (astNode 11 2 (PB.Body [] Nothing))))), astNode 1 12 (PB.MatchCase (astNode 7 4 (PB.PatternEnum (astNode 7 4 (PB.AccessName [astNode 7 4 (PB.Name "false")])) [])) (astNode 1 12 (PB.BlockExpr (astNode 1 12 (PB.Body [astNode 1 12 PB.Break] Nothing)))))]))),
    testAstOk "Parse while (3): statement with unary operator" "while x != 3 {}" A.pWhile (PB.Loop (astNode 1 15 (PB.Match (astNode 7 7 (PB.FunctionCall (astNode 9 3 (PB.Variable (astNode 9 3 (PB.AccessName [astNode 9 3 (PB.Name "logical_not")])))) (astNode 7 7 (funcCallArgs Nothing [astNode 7 7 (PB.FunctionCall (astNode 9 3 (PB.Variable (astNode 9 3 (PB.AccessName [astNode 9 3 (PB.Name "eq")])))) (astNode 7 7 (funcCallArgs Nothing [astNode 7 2 (PB.Variable (astNode 7 2 (PB.AccessName [astNode 7 2 (PB.Name "x")]))), astNode 12 2 (PB.LiteralExpr (astNode 12 2 (PB.IntLiteral 3)))])))])))) [astNode 1 15 (PB.MatchCase (astNode 7 7 (PB.PatternEnum (astNode 7 7 (PB.AccessName [astNode 7 7 (PB.Name "true")])) [])) (astNode 14 2 (PB.BlockExpr (astNode 14 2 (PB.Body [] Nothing))))), astNode 1 15 (PB.MatchCase (astNode 7 7 (PB.PatternEnum (astNode 7 7 (PB.AccessName [astNode 7 7 (PB.Name "false")])) [])) (astNode 1 15 (PB.BlockExpr (astNode 1 15 (PB.Body [astNode 1 15 PB.Break] Nothing)))))]))),
    testAstOk "Parse while (4): with condition func call" "while {do_something(x); x==3} {}" A.pWhile (PB.Loop (astNode 1 32 (PB.Match (astNode 7 24 (PB.BlockExpr (astNode 7 24 (PB.Body [astNode 8 15 (PB.ExprStatement (astNode 8 15 (PB.FunctionCall (astNode 8 12 (PB.Variable (astNode 8 12 (PB.AccessName [astNode 8 12 (PB.Name "do_something")])))) (astNode 20 3 (funcCallArgs Nothing [astNode 21 1 (PB.Variable (astNode 21 1 (PB.AccessName [astNode 21 1 (PB.Name "x")])))])))))] (Just (astNode 25 4 (PB.FunctionCall (astNode 26 2 (PB.Variable (astNode 26 2 (PB.AccessName [astNode 26 2 (PB.Name "eq")])))) (astNode 25 4 (funcCallArgs Nothing [astNode 25 1 (PB.Variable (astNode 25 1 (PB.AccessName [astNode 25 1 (PB.Name "x")]))), astNode 28 1 (PB.LiteralExpr (astNode 28 1 (PB.IntLiteral 3)))]))))))))) [astNode 1 32 (PB.MatchCase (astNode 7 24 (PB.PatternEnum (astNode 7 24 (PB.AccessName [astNode 7 24 (PB.Name "true")])) [])) (astNode 31 2 (PB.BlockExpr (astNode 31 2 (PB.Body [] Nothing))))), astNode 1 32 (PB.MatchCase (astNode 7 24 (PB.PatternEnum (astNode 7 24 (PB.AccessName [astNode 7 24 (PB.Name "false")])) [])) (astNode 1 32 (PB.BlockExpr (astNode 1 32 (PB.Body [astNode 1 32 PB.Break] Nothing)))))]))),
    testAstOk "Parse while (5): with condition func call" "while do_something(x) {}" A.pWhile (PB.Loop (astNode 1 24 (PB.Match (astNode 7 16 (PB.FunctionCall (astNode 7 12 (PB.Variable (astNode 7 12 (PB.AccessName [astNode 7 12 (PB.Name "do_something")])))) (astNode 19 4 (funcCallArgs Nothing [astNode 20 1 (PB.Variable (astNode 20 1 (PB.AccessName [astNode 20 1 (PB.Name "x")])))])))) [astNode 1 24 (PB.MatchCase (astNode 7 16 (PB.PatternEnum (astNode 7 16 (PB.AccessName [astNode 7 16 (PB.Name "true")])) [])) (astNode 23 2 (PB.BlockExpr (astNode 23 2 (PB.Body [] Nothing))))), astNode 1 24 (PB.MatchCase (astNode 7 16 (PB.PatternEnum (astNode 7 16 (PB.AccessName [astNode 7 16 (PB.Name "false")])) [])) (astNode 1 24 (PB.BlockExpr (astNode 1 24 (PB.Body [astNode 1 24 PB.Break] Nothing)))))]))),
    testAstOk "Parse while (6): with no block and empty condition" "while {} {}" A.pWhile (PB.Loop (astNode 1 11 (PB.Match (astNode 7 3 (PB.BlockExpr (astNode 7 3 (PB.Body [] Nothing)))) [astNode 1 11 (PB.MatchCase (astNode 7 3 (PB.PatternEnum (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "true")])) [])) (astNode 10 2 (PB.BlockExpr (astNode 10 2 (PB.Body [] Nothing))))), astNode 1 11 (PB.MatchCase (astNode 7 3 (PB.PatternEnum (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "false")])) [])) (astNode 1 11 (PB.BlockExpr (astNode 1 11 (PB.Body [astNode 1 11 PB.Break] Nothing)))))]))),
    testAstFail "Parse while (7): with no generator statement 3" "while {x {}" A.pWhile
  ]

-- | Test let statement parsing
letTests :: [Test]
letTests =
  [ testAstFail "Let statements must have a name associated with the variable declaration or function definition" "let : u8" A.pLet,
    testAstOk "Let statement for variable declaration" "let var: u8" A.pLet (PB.Let (astNode 5 3 (PB.PatternBinding (astNode 5 3 (PB.Name "var")))) Nothing (Just (astNode 10 2 (PB.NamedType (astNode 10 2 (PB.AccessName [astNode 10 2 (PB.Name "u8")])) []))) Nothing),
    testAstOk "Let statement for variable declaration with assignment" "let var: f32 = 6.5" A.pLet (PB.Let (astNode 5 3 (PB.PatternBinding (astNode 5 3 (PB.Name "var")))) Nothing (Just (astNode 10 4 (PB.NamedType (astNode 10 4 (PB.AccessName [astNode 10 4 (PB.Name "f32")])) []))) (Just (astNode 16 3 (PB.LiteralExpr (astNode 16 3 (PB.FloatLiteral 6.5)))))),
    testAstOk "Let statement for function" "let function = (a: str, b: str) => a == b" A.pLet (PB.Let (astNode 5 9 (PB.PatternBinding (astNode 5 9 (PB.Name "function")))) Nothing Nothing (Just (astNode 16 26 (PB.LiteralExpr (astNode 16 26 (PB.FunctionLiteral [astNode 17 6 (PB.FunctionParam (astNode 17 1 (PB.Name "a")) (Just (astNode 20 3 (PB.NamedType (astNode 20 3 (PB.AccessName [astNode 20 3 (PB.Name "str")])) [])))), astNode 25 6 (PB.FunctionParam (astNode 25 1 (PB.Name "b")) (Just (astNode 28 3 (PB.NamedType (astNode 28 3 (PB.AccessName [astNode 28 3 (PB.Name "str")])) []))))] Nothing (astNode 36 6 (PB.FunctionCall (astNode 38 3 (PB.Variable (astNode 38 3 (PB.AccessName [astNode 38 3 (PB.Name "eq")])))) (astNode 36 6 (funcCallArgs Nothing [astNode 36 2 (PB.Variable (astNode 36 2 (PB.AccessName [astNode 36 2 (PB.Name "a")]))), astNode 41 1 (PB.Variable (astNode 41 1 (PB.AccessName [astNode 41 1 (PB.Name "b")])))])))))))))),
    testAstOk "Let statement for function with type" "let function: (str, str) => bool = (a, b) => a == b;" A.pLet (PB.Let (astNode 5 8 (PB.PatternBinding (astNode 5 8 (PB.Name "function")))) Nothing (Just (astNode 15 19 (PB.FunctionType [astNode 16 3 (PB.NamedType (astNode 16 3 (PB.AccessName [astNode 16 3 (PB.Name "str")])) []), astNode 21 3 (PB.NamedType (astNode 21 3 (PB.AccessName [astNode 21 3 (PB.Name "str")])) [])] (astNode 29 5 (PB.NamedType (astNode 29 5 (PB.AccessName [astNode 29 5 (PB.Name "bool")])) []))))) (Just (astNode 36 16 (PB.LiteralExpr (astNode 36 16 (PB.FunctionLiteral [astNode 37 1 (PB.FunctionParam (astNode 37 1 (PB.Name "a")) Nothing), astNode 40 1 (PB.FunctionParam (astNode 40 1 (PB.Name "b")) Nothing)] Nothing (astNode 46 6 (PB.FunctionCall (astNode 48 3 (PB.Variable (astNode 48 3 (PB.AccessName [astNode 48 3 (PB.Name "eq")])))) (astNode 46 6 (funcCallArgs Nothing [astNode 46 2 (PB.Variable (astNode 46 2 (PB.AccessName [astNode 46 2 (PB.Name "a")]))), astNode 51 1 (PB.Variable (astNode 51 1 (PB.AccessName [astNode 51 1 (PB.Name "b")])))]))))))))))
  ]

-- | Test expression parsing
expressionTests :: [Test]
expressionTests =
  [ testAstOk
      "test expression 1"
      "1 + 1"
      A.pExpression
      ( PB.FunctionCall
          ( astNode
              3
              2
              ( PB.Variable
                  ( astNode
                      3
                      2
                      ( PB.AccessName
                          [ astNode 3 2 (PB.Name "add")
                          ]
                      )
                  )
              )
          )
          ( astNode
              1
              5
              ( funcCallArgs
                  Nothing
                  [ astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 1))),
                    astNode 5 1 (PB.LiteralExpr (astNode 5 1 (PB.IntLiteral 1)))
                  ]
              )
          )
      ),
    testAstOk
      "test expression typed 1"
      "(1 + 2 as f32) as str"
      A.pExpression
      $ PB.TypedExpression (astNode 1 15 (PB.FunctionCall (astNode 4 2 (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "add")])))) (astNode 2 12 (funcCallArgs Nothing [astNode 2 2 (PB.LiteralExpr (astNode 2 2 (PB.IntLiteral 1))), astNode 6 8 (PB.TypedExpression (astNode 6 2 (PB.LiteralExpr (astNode 6 2 (PB.IntLiteral 2)))) (astNode 11 3 (PB.NamedType (astNode 11 3 (PB.AccessName [astNode 11 3 (PB.Name "f32")])) [])))])))) (astNode 19 3 (PB.NamedType (astNode 19 3 (PB.AccessName [astNode 19 3 (PB.Name "str")])) [])),
    testAstOk
      "test expression typed 2"
      "function(x) as u8"
      A.pExpression
      $ PB.TypedExpression (astNode 1 12 (PB.FunctionCall (astNode 1 8 (PB.Variable (astNode 1 8 (PB.AccessName [astNode 1 8 (PB.Name "function")])))) (astNode 9 4 (funcCallArgs Nothing [astNode 10 1 (PB.Variable (astNode 10 1 (PB.AccessName [astNode 10 1 (PB.Name "x")])))])))) (astNode 16 2 (PB.NamedType (astNode 16 2 (PB.AccessName [astNode 16 2 (PB.Name "u8")])) [])),
    testAstOk
      "test expression typed 3"
      "function(x as str)"
      A.pExpression
      $ PB.FunctionCall (astNode 1 8 (PB.Variable (astNode 1 8 (PB.AccessName [astNode 1 8 (PB.Name "function")])))) (astNode 9 10 (funcCallArgs Nothing [astNode 10 8 (PB.TypedExpression (astNode 10 2 (PB.Variable (astNode 10 2 (PB.AccessName [astNode 10 2 (PB.Name "x")])))) (astNode 15 3 (PB.NamedType (astNode 15 3 (PB.AccessName [astNode 15 3 (PB.Name "str")])) [])))])),
    testAstOk
      "test expression typed 4"
      "((function[i] * 5) as f32) as str"
      A.pExpression
      $ PB.TypedExpression (astNode 1 27 (PB.TypedExpression (astNode 2 18 (PB.FunctionCall (astNode 15 2 (PB.Variable (astNode 15 2 (PB.AccessName [astNode 15 2 (PB.Name "mul")])))) (astNode 3 15 (funcCallArgs Nothing [astNode 3 12 (PB.FunctionCall (astNode 3 12 (PB.Variable (astNode 3 12 (PB.AccessName [astNode 3 12 (PB.Name "index")])))) (astNode 3 12 (funcCallArgs Nothing [astNode 3 8 (PB.Variable (astNode 3 8 (PB.AccessName [astNode 3 8 (PB.Name "function")]))), astNode 12 1 (PB.Variable (astNode 12 1 (PB.AccessName [astNode 12 1 (PB.Name "i")])))]))), astNode 17 1 (PB.LiteralExpr (astNode 17 1 (PB.IntLiteral 5)))])))) (astNode 23 3 (PB.NamedType (astNode 23 3 (PB.AccessName [astNode 23 3 (PB.Name "f32")])) [])))) (astNode 31 3 (PB.NamedType (astNode 31 3 (PB.AccessName [astNode 31 3 (PB.Name "str")])) [])),
    testAstOk
      "test expression typed precedence 1"
      "3 + 4 as str"
      A.pExpression
      $ PB.FunctionCall (astNode 3 2 (PB.Variable (astNode 3 2 (PB.AccessName [astNode 3 2 (PB.Name "add")])))) (astNode 1 12 (funcCallArgs Nothing [astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 3))), astNode 5 8 (PB.TypedExpression (astNode 5 2 (PB.LiteralExpr (astNode 5 2 (PB.IntLiteral 4)))) (astNode 10 3 (PB.NamedType (astNode 10 3 (PB.AccessName [astNode 10 3 (PB.Name "str")])) [])))])),
    testAstOk
      "test expression typed precedence 2"
      "1 as u8 + 2 as u8"
      A.pExpression
      $ PB.FunctionCall (astNode 9 2 (PB.Variable (astNode 9 2 (PB.AccessName [astNode 9 2 (PB.Name "add")])))) (astNode 1 17 (funcCallArgs Nothing [astNode 1 8 (PB.TypedExpression (astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 1)))) (astNode 6 3 (PB.NamedType (astNode 6 3 (PB.AccessName [astNode 6 3 (PB.Name "u8")])) []))), astNode 11 7 (PB.TypedExpression (astNode 11 2 (PB.LiteralExpr (astNode 11 2 (PB.IntLiteral 2)))) (astNode 16 2 (PB.NamedType (astNode 16 2 (PB.AccessName [astNode 16 2 (PB.Name "u8")])) [])))])),
    testAstOk
      "test expression 2"
      "function(x)"
      A.pExpression
      ( PB.FunctionCall
          ( astNode
              1
              8
              ( PB.Variable
                  ( astNode
                      1
                      8
                      ( PB.AccessName
                          [ astNode 1 8 (PB.Name "function")
                          ]
                      )
                  )
              )
          )
          ( astNode
              9
              3
              ( funcCallArgs
                  Nothing
                  [ astNode
                      10
                      1
                      ( PB.Variable
                          ( astNode
                              10
                              1
                              ( PB.AccessName
                                  [ astNode 10 1 (PB.Name "x")
                                  ]
                              )
                          )
                      )
                  ]
              )
          )
      ),
    testAstOk
      "test expression 3"
      "dog.bark()"
      A.pExpression
      $ PB.FunctionCall
        ( astNode
            5
            4
            ( PB.Variable
                ( astNode
                    5
                    4
                    ( PB.AccessName
                        [ astNode 5 4 (PB.Name "bark")
                        ]
                    )
                )
            )
        )
        ( astNode
            1
            10
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    3
                    ( PB.Variable
                        ( astNode
                            1
                            3
                            ( PB.AccessName
                                [ astNode 1 3 (PB.Name "dog")
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk
      "test expression 4"
      "dog.bark_with(something)"
      A.pExpression
      $ PB.FunctionCall
        ( astNode
            5
            9
            ( PB.Variable
                ( astNode
                    5
                    9
                    ( PB.AccessName
                        [ astNode 5 9 (PB.Name "bark_with")
                        ]
                    )
                )
            )
        )
        ( astNode
            1
            24
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    3
                    ( PB.Variable
                        ( astNode
                            1
                            3
                            ( PB.AccessName
                                [ astNode 1 3 (PB.Name "dog")
                                ]
                            )
                        )
                    ),
                  astNode
                    15
                    9
                    ( PB.Variable
                        ( astNode
                            15
                            9
                            ( PB.AccessName
                                [ astNode 15 9 (PB.Name "something")
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk
      "test expression 5"
      "1 + 1 * 1"
      A.pExpression
      $ PB.FunctionCall
        ( astNode
            3
            2
            ( PB.Variable
                ( astNode
                    3
                    2
                    ( PB.AccessName
                        [ astNode 3 2 (PB.Name "add")
                        ]
                    )
                )
            )
        )
        ( astNode
            1
            9
            ( funcCallArgs
                Nothing
                [ astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 1))),
                  astNode
                    5
                    5
                    ( PB.FunctionCall
                        ( astNode
                            7
                            2
                            ( PB.Variable
                                ( astNode
                                    7
                                    2
                                    ( PB.AccessName
                                        [ astNode 7 2 (PB.Name "mul")
                                        ]
                                    )
                                )
                            )
                        )
                        ( astNode
                            5
                            5
                            ( funcCallArgs
                                Nothing
                                [ astNode 5 2 (PB.LiteralExpr (astNode 5 2 (PB.IntLiteral 1))),
                                  astNode 9 1 (PB.LiteralExpr (astNode 9 1 (PB.IntLiteral 1)))
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk
      "test expression 6"
      "1 * 1 + 1"
      A.pExpression
      $ PB.FunctionCall
        ( astNode
            7
            2
            ( PB.Variable
                ( astNode
                    7
                    2
                    ( PB.AccessName
                        [ astNode 7 2 (PB.Name "add")
                        ]
                    )
                )
            )
        )
        ( astNode
            1
            9
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    6
                    ( PB.FunctionCall
                        ( astNode
                            3
                            2
                            ( PB.Variable
                                ( astNode
                                    3
                                    2
                                    ( PB.AccessName
                                        [ astNode 3 2 (PB.Name "mul")
                                        ]
                                    )
                                )
                            )
                        )
                        ( astNode
                            1
                            6
                            ( funcCallArgs
                                Nothing
                                [ astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 1))),
                                  astNode 5 2 (PB.LiteralExpr (astNode 5 2 (PB.IntLiteral 1)))
                                ]
                            )
                        )
                    ),
                  astNode 9 1 (PB.LiteralExpr (astNode 9 1 (PB.IntLiteral 1)))
                ]
            )
        ),
    testAstOk
      "test expression 7"
      "!a"
      A.pExpression
      $ PB.FunctionCall
        ( astNode
            1
            1
            ( PB.Variable
                ( astNode
                    1
                    1
                    ( PB.AccessName
                        [ astNode 1 1 (PB.Name "not")
                        ]
                    )
                )
            )
        )
        ( astNode
            2
            1
            ( funcCallArgs
                Nothing
                [ astNode
                    2
                    1
                    ( PB.Variable
                        ( astNode
                            2
                            1
                            ( PB.AccessName
                                [ astNode 2 1 (PB.Name "a")
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk
      "test expression 8"
      "!~b"
      A.pExpression
      $ PB.FunctionCall
        (astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "not")]))))
        ( astNode
            2
            2
            ( funcCallArgs
                Nothing
                [ astNode
                    2
                    2
                    ( PB.FunctionCall
                        (astNode 2 1 (PB.Variable (astNode 2 1 (PB.AccessName [astNode 2 1 (PB.Name "bit_not")]))))
                        ( astNode
                            3
                            1
                            ( funcCallArgs
                                Nothing
                                [astNode 3 1 (PB.Variable (astNode 3 1 (PB.AccessName [astNode 3 1 (PB.Name "b")])))]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk
      "test expression 9"
      "!(a + b)"
      A.pExpression
      $ PB.FunctionCall
        ( astNode
            1
            1
            ( PB.Variable
                ( astNode
                    1
                    1
                    ( PB.AccessName
                        [ astNode 1 1 (PB.Name "not")
                        ]
                    )
                )
            )
        )
        ( astNode
            2
            7
            ( funcCallArgs
                Nothing
                [ astNode
                    2
                    7
                    ( PB.FunctionCall
                        ( astNode
                            5
                            2
                            ( PB.Variable
                                ( astNode
                                    5
                                    2
                                    ( PB.AccessName
                                        [ astNode 5 2 (PB.Name "add")
                                        ]
                                    )
                                )
                            )
                        )
                        ( astNode
                            3
                            5
                            ( funcCallArgs
                                Nothing
                                [ astNode
                                    3
                                    2
                                    ( PB.Variable
                                        ( astNode
                                            3
                                            2
                                            ( PB.AccessName
                                                [ astNode 3 2 (PB.Name "a")
                                                ]
                                            )
                                        )
                                    ),
                                  astNode
                                    7
                                    1
                                    ( PB.Variable
                                        ( astNode
                                            7
                                            1
                                            ( PB.AccessName
                                                [ astNode 7 1 (PB.Name "b")
                                                ]
                                            )
                                        )
                                    )
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk
      "test expression 10"
      "!(a + b).fn_call()"
      A.pExpression
      $ PB.FunctionCall
        ( astNode
            1
            1
            ( PB.Variable
                ( astNode
                    1
                    1
                    ( PB.AccessName
                        [ astNode 1 1 (PB.Name "not")
                        ]
                    )
                )
            )
        )
        ( astNode
            2
            17
            ( funcCallArgs
                Nothing
                [ astNode
                    2
                    17
                    ( PB.FunctionCall
                        ( astNode
                            10
                            7
                            ( PB.Variable
                                ( astNode
                                    10
                                    7
                                    ( PB.AccessName
                                        [ astNode 10 7 (PB.Name "fn_call")
                                        ]
                                    )
                                )
                            )
                        )
                        ( astNode
                            2
                            17
                            ( funcCallArgs
                                Nothing
                                [ astNode
                                    2
                                    7
                                    ( PB.FunctionCall
                                        ( astNode
                                            5
                                            2
                                            ( PB.Variable
                                                ( astNode
                                                    5
                                                    2
                                                    ( PB.AccessName
                                                        [ astNode 5 2 (PB.Name "add")
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                        ( astNode
                                            3
                                            5
                                            ( funcCallArgs
                                                Nothing
                                                [ astNode
                                                    3
                                                    2
                                                    ( PB.Variable
                                                        ( astNode
                                                            3
                                                            2
                                                            ( PB.AccessName
                                                                [ astNode 3 2 (PB.Name "a")
                                                                ]
                                                            )
                                                        )
                                                    ),
                                                  astNode
                                                    7
                                                    1
                                                    ( PB.Variable
                                                        ( astNode
                                                            7
                                                            1
                                                            ( PB.AccessName
                                                                [ astNode 7 1 (PB.Name "b")
                                                                ]
                                                            )
                                                        )
                                                    )
                                                ]
                                            )
                                        )
                                    )
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk
      "test expression 11"
      "a(b.c().d()).e()"
      A.pExpression
      $ PB.FunctionCall
        ( astNode
            14
            1
            ( PB.Variable
                ( astNode
                    14
                    1
                    ( PB.AccessName
                        [ astNode 14 1 (PB.Name "e")
                        ]
                    )
                )
            )
        )
        ( astNode
            1
            16
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    12
                    ( PB.FunctionCall
                        ( astNode
                            1
                            1
                            ( PB.Variable
                                ( astNode
                                    1
                                    1
                                    ( PB.AccessName
                                        [ astNode 1 1 (PB.Name "a")
                                        ]
                                    )
                                )
                            )
                        )
                        ( astNode
                            2
                            11
                            ( funcCallArgs
                                Nothing
                                [ astNode
                                    3
                                    9
                                    ( PB.FunctionCall
                                        ( astNode
                                            9
                                            1
                                            ( PB.Variable
                                                ( astNode
                                                    9
                                                    1
                                                    ( PB.AccessName
                                                        [ astNode 9 1 (PB.Name "d")
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                        ( astNode
                                            3
                                            9
                                            ( funcCallArgs
                                                Nothing
                                                [ astNode
                                                    3
                                                    5
                                                    ( PB.FunctionCall
                                                        ( astNode
                                                            5
                                                            1
                                                            ( PB.Variable
                                                                ( astNode
                                                                    5
                                                                    1
                                                                    ( PB.AccessName
                                                                        [ astNode 5 1 (PB.Name "c")
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                        ( astNode
                                                            3
                                                            5
                                                            ( funcCallArgs
                                                                Nothing
                                                                [ astNode
                                                                    3
                                                                    1
                                                                    ( PB.Variable
                                                                        ( astNode
                                                                            3
                                                                            1
                                                                            ( PB.AccessName
                                                                                [ astNode 3 1 (PB.Name "b")
                                                                                ]
                                                                            )
                                                                        )
                                                                    )
                                                                ]
                                                            )
                                                        )
                                                    )
                                                ]
                                            )
                                        )
                                    )
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk "test expression 12" "dog.name.print()" A.pExpression $
      PB.FunctionCall
        ( astNode
            10
            5
            ( PB.Variable
                ( astNode
                    10
                    5
                    ( PB.AccessName
                        [ astNode 10 5 (PB.Name "print")
                        ]
                    )
                )
            )
        )
        ( astNode
            1
            16
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    8
                    ( PB.PropertyAccess
                        (astNode 1 3 (PB.Variable (astNode 1 3 (PB.AccessName [astNode 1 3 (PB.Name "dog")]))))
                        (astNode 5 4 (PB.Name "name"))
                    )
                ]
            )
        ),
    testAstOk "test expression 13" "(whatever()).something" A.pExpression $
      PB.PropertyAccess
        ( astNode
            1
            12
            ( PB.FunctionCall
                ( astNode
                    2
                    8
                    ( PB.Variable
                        ( astNode
                            2
                            8
                            ( PB.AccessName
                                [ astNode 2 8 (PB.Name "whatever")
                                ]
                            )
                        )
                    )
                )
                ( astNode
                    10
                    2
                    ( funcCallArgs
                        Nothing
                        []
                    )
                )
            )
        )
        (astNode 14 9 (PB.Name "something")),
    testAstOk "test expression 14" "(whatever()) + 3 * 4" A.pExpression $
      PB.FunctionCall
        ( astNode
            14
            2
            ( PB.Variable
                ( astNode
                    14
                    2
                    ( PB.AccessName
                        [ astNode 14 2 (PB.Name "add")
                        ]
                    )
                )
            )
        )
        ( astNode
            1
            20
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    13
                    ( PB.FunctionCall
                        (astNode 2 8 (PB.Variable (astNode 2 8 (PB.AccessName [astNode 2 8 (PB.Name "whatever")]))))
                        (astNode 10 2 (funcCallArgs Nothing []))
                    ),
                  astNode
                    16
                    5
                    ( PB.FunctionCall
                        (astNode 18 2 (PB.Variable (astNode 18 2 (PB.AccessName [astNode 18 2 (PB.Name "mul")]))))
                        ( astNode
                            16
                            5
                            ( funcCallArgs
                                Nothing
                                [ astNode 16 2 (PB.LiteralExpr (astNode 16 2 (PB.IntLiteral 3))),
                                  astNode 20 1 (PB.LiteralExpr (astNode 20 1 (PB.IntLiteral 4)))
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk "test expression 15" "1 < 3" A.pExpression $
      PB.FunctionCall
        ( astNode
            3
            2
            ( PB.Variable
                ( astNode
                    3
                    2
                    ( PB.AccessName
                        [ astNode 3 2 (PB.Name "eq")
                        ]
                    )
                )
            )
        )
        ( astNode
            1
            5
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    5
                    ( PB.FunctionCall
                        ( astNode
                            3
                            2
                            ( PB.Variable
                                ( astNode
                                    3
                                    2
                                    ( PB.AccessName
                                        [ astNode 3 2 (PB.Name "ord")
                                        ]
                                    )
                                )
                            )
                        )
                        ( astNode
                            1
                            5
                            ( funcCallArgs
                                Nothing
                                [ astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 1))),
                                  astNode 5 1 (PB.LiteralExpr (astNode 5 1 (PB.IntLiteral 3)))
                                ]
                            )
                        )
                    ),
                  astNode
                    1
                    5
                    ( PB.Variable
                        ( astNode
                            3
                            2
                            ( PB.AccessName
                                [ astNode 3 2 (PB.Name "Lt")
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk "test expression 16" "1 < 3 >= 4" A.pExpression (PB.FunctionCall (astNode 3 2 (PB.Variable (astNode 3 2 (PB.AccessName [astNode 3 2 (PB.Name "eq")])))) (astNode 1 10 (PB.FunctionCallArgs Nothing [astNode 1 10 (PB.FunctionCall (astNode 3 2 (PB.Variable (astNode 3 2 (PB.AccessName [astNode 3 2 (PB.Name "ord")])))) (astNode 1 10 (PB.FunctionCallArgs Nothing [astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 1))), astNode 5 6 (PB.BlockExpr (astNode 5 6 (PB.Body [astNode 7 3 (PB.Let (astNode 7 3 (PB.PatternBinding (astNode 7 3 (PB.Name "$val")))) Nothing Nothing (Just (astNode 5 6 (PB.FunctionCall (astNode 7 3 (PB.Variable (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "ord")])))) (astNode 5 6 (PB.FunctionCallArgs Nothing [astNode 5 2 (PB.LiteralExpr (astNode 5 2 (PB.IntLiteral 3))), astNode 10 1 (PB.LiteralExpr (astNode 10 1 (PB.IntLiteral 4)))] "<unnamed>"))))))] (Just (astNode 5 6 (PB.LogicalOp PB.LogicalOrOp (astNode 5 6 (PB.FunctionCall (astNode 7 3 (PB.Variable (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "eq")])))) (astNode 5 6 (PB.FunctionCallArgs Nothing [astNode 5 6 (PB.Variable (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "$val")]))), astNode 5 6 (PB.Variable (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "Eq")])))] "<unnamed>")))) (astNode 5 6 (PB.FunctionCall (astNode 7 3 (PB.Variable (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "eq")])))) (astNode 5 6 (PB.FunctionCallArgs Nothing [astNode 5 6 (PB.Variable (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "$val")]))), astNode 5 6 (PB.Variable (astNode 7 3 (PB.AccessName [astNode 7 3 (PB.Name "Gt")])))] "<unnamed>"))))))))))] "<unnamed>"))), astNode 1 10 (PB.Variable (astNode 3 2 (PB.AccessName [astNode 3 2 (PB.Name "Lt")])))] "<unnamed>"))),
    testAstOk "test expression 17" "-3" A.pExpression $
      PB.FunctionCall
        (astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "neg")]))))
        (astNode 2 1 (funcCallArgs Nothing [astNode 2 1 (PB.LiteralExpr (astNode 2 1 (PB.IntLiteral 3)))])),
    testAstOk "test expression 18" "- 3" A.pExpression $
      PB.FunctionCall
        (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "neg")]))))
        (astNode 3 1 (funcCallArgs Nothing [astNode 3 1 (PB.LiteralExpr (astNode 3 1 (PB.IntLiteral 3)))])),
    testAstOk "test expression 19" "1 + - 3" A.pExpression $
      PB.FunctionCall
        (astNode 3 2 (PB.Variable (astNode 3 2 (PB.AccessName [astNode 3 2 (PB.Name "add")]))))
        ( astNode
            1
            7
            ( funcCallArgs
                Nothing
                [ astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 1))),
                  astNode
                    5
                    3
                    ( PB.FunctionCall
                        (astNode 5 2 (PB.Variable (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "neg")]))))
                        (astNode 7 1 (funcCallArgs Nothing [astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 3)))]))
                    )
                ]
            )
        ),
    testAstOk "test expression 20" "1 + -3" A.pExpression $
      PB.FunctionCall
        (astNode 3 2 (PB.Variable (astNode 3 2 (PB.AccessName [astNode 3 2 (PB.Name "add")]))))
        ( astNode
            1
            6
            ( funcCallArgs
                Nothing
                [ astNode 1 2 (PB.LiteralExpr (astNode 1 2 (PB.IntLiteral 1))),
                  astNode
                    5
                    2
                    ( PB.FunctionCall
                        (astNode 5 1 (PB.Variable (astNode 5 1 (PB.AccessName [astNode 5 1 (PB.Name "neg")]))))
                        (astNode 6 1 (funcCallArgs Nothing [astNode 6 1 (PB.LiteralExpr (astNode 6 1 (PB.IntLiteral 3)))]))
                    )
                ]
            )
        ),
    testAstOk "test expression 21" "-3 + -1" A.pExpression $
      PB.FunctionCall
        (astNode 4 2 (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "add")]))))
        ( astNode
            1
            7
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    3
                    ( PB.FunctionCall
                        (astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "neg")]))))
                        (astNode 2 2 (funcCallArgs Nothing [astNode 2 2 (PB.LiteralExpr (astNode 2 2 (PB.IntLiteral 3)))]))
                    ),
                  astNode
                    6
                    2
                    ( PB.FunctionCall
                        (astNode 6 1 (PB.Variable (astNode 6 1 (PB.AccessName [astNode 6 1 (PB.Name "neg")]))))
                        (astNode 7 1 (funcCallArgs Nothing [astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 1)))]))
                    )
                ]
            )
        ),
    testAstOk "test expression 22" "-a + -b" A.pExpression $
      PB.FunctionCall
        ( astNode
            4
            2
            (PB.Variable (astNode 4 2 (PB.AccessName [astNode 4 2 (PB.Name "add")])))
        )
        ( astNode
            1
            7
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    3
                    ( PB.FunctionCall
                        (astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "neg")]))))
                        ( astNode 2 2 (funcCallArgs Nothing [astNode 2 2 (PB.Variable (astNode 2 2 (PB.AccessName [astNode 2 2 (PB.Name "a")])))])
                        )
                    ),
                  astNode
                    6
                    2
                    ( PB.FunctionCall
                        (astNode 6 1 (PB.Variable (astNode 6 1 (PB.AccessName [astNode 6 1 (PB.Name "neg")]))))
                        (astNode 7 1 (funcCallArgs Nothing [astNode 7 1 (PB.Variable (astNode 7 1 (PB.AccessName [astNode 7 1 (PB.Name "b")])))]))
                    )
                ]
            )
        ),
    testAstOk "test expression 23" "- a + - b" A.pExpression $
      PB.FunctionCall
        (astNode 5 2 (PB.Variable (astNode 5 2 (PB.AccessName [astNode 5 2 (PB.Name "add")]))))
        ( astNode
            1
            9
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    4
                    ( PB.FunctionCall
                        (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "neg")]))))
                        ( astNode 3 2 (funcCallArgs Nothing [astNode 3 2 (PB.Variable (astNode 3 2 (PB.AccessName [astNode 3 2 (PB.Name "a")])))])
                        )
                    ),
                  astNode
                    7
                    3
                    ( PB.FunctionCall
                        (astNode 7 2 (PB.Variable (astNode 7 2 (PB.AccessName [astNode 7 2 (PB.Name "neg")]))))
                        (astNode 9 1 (funcCallArgs Nothing [astNode 9 1 (PB.Variable (astNode 9 1 (PB.AccessName [astNode 9 1 (PB.Name "b")])))]))
                    )
                ]
            )
        ),
    testAstOk "test expression 24" "a[3]" A.pExpression $
      PB.FunctionCall
        (astNode 1 4 (PB.Variable (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "index")]))))
        ( astNode
            1
            4
            ( funcCallArgs
                Nothing
                [ astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "a")]))),
                  astNode 3 1 (PB.LiteralExpr (astNode 3 1 (PB.IntLiteral 3)))
                ]
            )
        ),
    testAstOk "test expression 25" "one[two][three](four)" A.pExpression $
      PB.FunctionCall
        ( astNode
            1
            15
            ( PB.FunctionCall
                (astNode 1 15 (PB.Variable (astNode 1 15 (PB.AccessName [astNode 1 15 (PB.Name "index")]))))
                ( astNode
                    1
                    15
                    ( funcCallArgs
                        Nothing
                        [ astNode
                            1
                            8
                            ( PB.FunctionCall
                                (astNode 1 8 (PB.Variable (astNode 1 8 (PB.AccessName [astNode 1 8 (PB.Name "index")]))))
                                ( astNode
                                    1
                                    8
                                    ( funcCallArgs
                                        Nothing
                                        [ astNode 1 3 (PB.Variable (astNode 1 3 (PB.AccessName [astNode 1 3 (PB.Name "one")]))),
                                          astNode 5 3 (PB.Variable (astNode 5 3 (PB.AccessName [astNode 5 3 (PB.Name "two")])))
                                        ]
                                    )
                                )
                            ),
                          astNode 10 5 (PB.Variable (astNode 10 5 (PB.AccessName [astNode 10 5 (PB.Name "three")])))
                        ]
                    )
                )
            )
        )
        (astNode 16 6 (funcCallArgs Nothing [astNode 17 4 (PB.Variable (astNode 17 4 (PB.AccessName [astNode 17 4 (PB.Name "four")])))])),
    testAstOk "test expression 26" "(one.two[three].four(five))[(six)]" A.pExpression $
      PB.FunctionCall
        (astNode 1 34 (PB.Variable (astNode 1 34 (PB.AccessName [astNode 1 34 (PB.Name "index")]))))
        ( astNode
            1
            34
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    27
                    ( PB.FunctionCall
                        (astNode 17 4 (PB.Variable (astNode 17 4 (PB.AccessName [astNode 17 4 (PB.Name "four")]))))
                        ( astNode
                            2
                            25
                            ( funcCallArgs
                                Nothing
                                [ astNode
                                    2
                                    14
                                    ( PB.FunctionCall
                                        (astNode 2 14 (PB.Variable (astNode 2 14 (PB.AccessName [astNode 2 14 (PB.Name "index")]))))
                                        ( astNode
                                            2
                                            14
                                            ( funcCallArgs
                                                Nothing
                                                [ astNode
                                                    2
                                                    14
                                                    ( PB.PropertyAccess
                                                        (astNode 2 3 (PB.Variable (astNode 2 3 (PB.AccessName [astNode 2 3 (PB.Name "one")]))))
                                                        (astNode 6 3 (PB.Name "two"))
                                                    ),
                                                  astNode 10 5 (PB.Variable (astNode 10 5 (PB.AccessName [astNode 10 5 (PB.Name "three")])))
                                                ]
                                            )
                                        )
                                    ),
                                  astNode 22 4 (PB.Variable (astNode 22 4 (PB.AccessName [astNode 22 4 (PB.Name "five")])))
                                ]
                            )
                        )
                    ),
                  astNode 29 5 (PB.Variable (astNode 30 3 (PB.AccessName [astNode 30 3 (PB.Name "six")])))
                ]
            )
        ),
    testAstOk "test expression 27" "!one[two]" A.pExpression $
      PB.FunctionCall
        (astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "not")]))))
        ( astNode
            2
            8
            ( funcCallArgs
                Nothing
                [ astNode
                    2
                    8
                    ( PB.FunctionCall
                        (astNode 2 8 (PB.Variable (astNode 2 8 (PB.AccessName [astNode 2 8 (PB.Name "index")]))))
                        ( astNode
                            2
                            8
                            ( funcCallArgs
                                Nothing
                                [ astNode 2 3 (PB.Variable (astNode 2 3 (PB.AccessName [astNode 2 3 (PB.Name "one")]))),
                                  astNode 6 3 (PB.Variable (astNode 6 3 (PB.AccessName [astNode 6 3 (PB.Name "two")])))
                                ]
                            )
                        )
                    )
                ]
            )
        ),
    testAstOk "test expression 28" "a + b[4]" A.pExpression $
      PB.FunctionCall
        (astNode 3 2 (PB.Variable (astNode 3 2 (PB.AccessName [astNode 3 2 (PB.Name "add")]))))
        ( astNode
            1
            8
            ( funcCallArgs
                Nothing
                [ astNode
                    1
                    2
                    (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")]))),
                  astNode
                    5
                    4
                    ( PB.FunctionCall
                        (astNode 5 4 (PB.Variable (astNode 5 4 (PB.AccessName [astNode 5 4 (PB.Name "index")]))))
                        ( astNode
                            5
                            4
                            ( funcCallArgs
                                Nothing
                                [ astNode 5 1 (PB.Variable (astNode 5 1 (PB.AccessName [astNode 5 1 (PB.Name "b")]))),
                                  astNode 7 1 (PB.LiteralExpr (astNode 7 1 (PB.IntLiteral 4)))
                                ]
                            )
                        )
                    )
                ]
            )
        )
  ]

-- | Test block parsing
blockTests :: [Test]
blockTests =
  [ testAstOk "test body block 1" "{ function_call(); 4 }" A.pBlock (PB.Body [astNode 3 15 (PB.ExprStatement (astNode 3 15 (PB.FunctionCall (astNode 3 13 (PB.Variable (astNode 3 13 (PB.AccessName [astNode 3 13 (PB.Name "function_call")])))) (astNode 16 2 (funcCallArgs Nothing [])))))] (Just (astNode 20 2 (PB.LiteralExpr (astNode 20 2 (PB.IntLiteral 4)))))),
    testAstOk "test body block 2" "{ x; x }" A.pBlock (PB.Body [astNode 3 1 (PB.ExprStatement (astNode 3 1 (PB.Variable (astNode 3 1 (PB.AccessName [astNode 3 1 (PB.Name "x")])))))] (Just (astNode 6 2 (PB.Variable (astNode 6 2 (PB.AccessName [astNode 6 2 (PB.Name "x")])))))),
    testAstOk "test body block 3" "{ one }" A.pBlock (PB.Body [] (Just (astNode 3 4 (PB.Variable (astNode 3 4 (PB.AccessName [astNode 3 4 (PB.Name "one")])))))),
    testAstOk "test body block 4" "{ one; two; three; four }" A.pBlock (PB.Body [astNode 3 3 (PB.ExprStatement (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "one")]))))), astNode 8 3 (PB.ExprStatement (astNode 8 3 (PB.Variable (astNode 8 3 (PB.AccessName [astNode 8 3 (PB.Name "two")]))))), astNode 13 5 (PB.ExprStatement (astNode 13 5 (PB.Variable (astNode 13 5 (PB.AccessName [astNode 13 5 (PB.Name "three")])))))] (Just (astNode 20 5 (PB.Variable (astNode 20 5 (PB.AccessName [astNode 20 5 (PB.Name "four")])))))),
    testAstFail "test body block 5" "{ ; x }" A.pBlock,
    testAstFail "test body block 5" "{ ; }" A.pBlock,
    testAstOk "test body block 5" "{ one; two; three; }" A.pBlock (PB.Body [astNode 3 3 (PB.ExprStatement (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "one")]))))), astNode 8 3 (PB.ExprStatement (astNode 8 3 (PB.Variable (astNode 8 3 (PB.AccessName [astNode 8 3 (PB.Name "two")]))))), astNode 13 5 (PB.ExprStatement (astNode 13 5 (PB.Variable (astNode 13 5 (PB.AccessName [astNode 13 5 (PB.Name "three")])))))] Nothing),
    testAstOk "test match block 1" "{ one; two; three; four }" A.pBlock (PB.Body [astNode 3 3 (PB.ExprStatement (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "one")]))))), astNode 8 3 (PB.ExprStatement (astNode 8 3 (PB.Variable (astNode 8 3 (PB.AccessName [astNode 8 3 (PB.Name "two")]))))), astNode 13 5 (PB.ExprStatement (astNode 13 5 (PB.Variable (astNode 13 5 (PB.AccessName [astNode 13 5 (PB.Name "three")])))))] (Just (astNode 20 5 (PB.Variable (astNode 20 5 (PB.AccessName [astNode 20 5 (PB.Name "four")]))))))
  ]

-- | import statement tests
importStatementTests :: [Test]
importStatementTests =
  [ testAstOk "basic import" "import(\"lib\");" A.pStatement (PB.ExprStatement (astNode 1 13 (PB.Import (astNode 1 13 (PB.StrLiteral "lib"))))),
    testAstOk "import with empty string literal" "import(\"\");" A.pStatement (PB.ExprStatement (astNode 1 10 (PB.Import (astNode 1 10 (PB.StrLiteral ""))))),
    testAstFail "unclosed string literal import" "import(\"lib);" A.pImport,
    testAstFail "non-string literal import" "import(lib);" A.pImport
  ]

-- | Test pattern parsing
patternTests :: [Test]
patternTests =
  [ testAstOk "test pattern binding" "moonshine" A.pPattern $ PB.PatternBinding (astNode 1 9 $ PB.Name "moonshine"),
    testAstOk "test pattern ignore" "_" A.pPattern PB.PatternIgnore,
    testAstOk "test pattern grouping" "(mypattern)" A.pPattern $ PB.PatternBinding (astNode 2 9 $ PB.Name "mypattern"),
    testAstOk "test pattern multiple 1" "first | second" A.pPattern $
      PB.PatternOr
        (astNode 1 6 $ PB.PatternBinding (astNode 1 6 $ PB.Name "first"))
        (astNode 9 6 $ PB.PatternBinding (astNode 9 6 $ PB.Name "second")),
    testAstOk "test pattern multiple 2" "first | second | third" A.pPattern $
      PB.PatternOr
        (astNode 1 6 $ PB.PatternBinding (astNode 1 6 $ PB.Name "first"))
        ( astNode 9 14 $
            PB.PatternOr
              (astNode 9 7 $ PB.PatternBinding (astNode 9 7 $ PB.Name "second"))
              (astNode 18 5 $ PB.PatternBinding (astNode 18 5 $ PB.Name "third"))
        ),
    testAstOk "test pattern multiple 3" "(first if second) | third" A.pPattern $
      PB.PatternOr
        ( astNode
            2
            15
            ( PB.PatternIf
                (astNode 2 6 (PB.PatternBinding (astNode 2 6 (PB.Name "first"))))
                (astNode 11 6 (PB.Variable (astNode 11 6 (PB.AccessName [astNode 11 6 (PB.Name "second")]))))
            )
        )
        (astNode 21 5 (PB.PatternBinding (astNode 21 5 (PB.Name "third")))),
    testAstOk "test pattern multiple 4" "first if second | third" A.pPattern $
      PB.PatternOr
        ( astNode
            1
            16
            ( PB.PatternIf
                (astNode 1 6 (PB.PatternBinding (astNode 1 6 (PB.Name "first"))))
                (astNode 10 7 (PB.Variable (astNode 10 7 (PB.AccessName [astNode 10 7 (PB.Name "second")]))))
            )
        )
        (astNode 19 5 (PB.PatternBinding (astNode 19 5 (PB.Name "third")))),
    testAstOk "test pattern multiple 5" "first if (second | third)" A.pPattern $
      PB.PatternIf
        (astNode 1 6 (PB.PatternBinding (astNode 1 6 (PB.Name "first"))))
        ( astNode
            10
            16
            ( PB.FunctionCall
                (astNode 18 2 (PB.Variable (astNode 18 2 (PB.AccessName [astNode 18 2 (PB.Name "bit_or")]))))
                ( astNode
                    11
                    14
                    ( funcCallArgs
                        Nothing
                        [ astNode 11 7 (PB.Variable (astNode 11 7 (PB.AccessName [astNode 11 7 (PB.Name "second")]))),
                          astNode 20 5 (PB.Variable (astNode 20 5 (PB.AccessName [astNode 20 5 (PB.Name "third")])))
                        ]
                    )
                )
            )
        ),
    -- enum matching tests
    testAstFail "test enum patterns 1" "Some()" (A.pEnumPattern <* M.eof),
    testAstFail "test enum patterns 2" "Some(a,)" (A.pEnumPattern <* M.eof),
    testAstFail "test enum patterns 3" "Some(a,b,)" (A.pEnumPattern <* M.eof),
    testAstFail "test enum patterns 4" "Some(a,b,c,,)" (A.pEnumPattern <* M.eof),
    testAstOk "test enum patterns 5" "Some(a)" A.pPattern (PB.PatternEnum (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Some")])) [astNode 6 1 (PB.PatternBinding (astNode 6 1 (PB.Name "a")))]),
    testAstOk "test enum patterns 6" "Some(a,b)" A.pPattern (PB.PatternEnum (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Some")])) [astNode 6 1 (PB.PatternBinding (astNode 6 1 (PB.Name "a"))), astNode 8 1 (PB.PatternBinding (astNode 8 1 (PB.Name "b")))]),
    -- struct matching tests
    testAstFail "test struct patterns 1" "{}" A.pStructPattern,
    testAstFail "test struct patterns 2" "{a:;}" A.pStructPattern,
    testAstFail "test struct patterns 3" "{a,}" A.pStructPattern,
    testAstOk "test empty struct pattern" "Dog {}" A.pPattern (PB.PatternStruct (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Dog")])) []),
    testAstOk "test single field struct pattern" "Dog {a}" A.pPattern (PB.PatternStruct (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Dog")])) [astNode 6 1 (PB.DestructuringEntry (astNode 6 1 (PB.Name "a")) (astNode 6 1 (PB.PatternBinding (astNode 6 1 (PB.Name "a")))))]),
    testAstOk "test single field struct pattern" "Dog {a;}" A.pPattern (PB.PatternStruct (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Dog")])) [astNode 6 1 (PB.DestructuringEntry (astNode 6 1 (PB.Name "a")) (astNode 6 1 (PB.PatternBinding (astNode 6 1 (PB.Name "a")))))]),
    testAstOk "test multiple struct pattern" "Dog {a; b}" A.pPattern (PB.PatternStruct (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Dog")])) [astNode 6 1 (PB.DestructuringEntry (astNode 6 1 (PB.Name "a")) (astNode 6 1 (PB.PatternBinding (astNode 6 1 (PB.Name "a"))))), astNode 9 1 (PB.DestructuringEntry (astNode 9 1 (PB.Name "b")) (astNode 9 1 (PB.PatternBinding (astNode 9 1 (PB.Name "b")))))]),
    testAstOk "test multiple struct pattern with re-assignment" "Dog {a: d; b}" A.pPattern (PB.PatternStruct (astNode 1 4 (PB.AccessName [astNode 1 4 (PB.Name "Dog")])) [astNode 6 4 (PB.DestructuringEntry (astNode 6 1 (PB.Name "a")) (astNode 9 1 (PB.PatternBinding (astNode 9 1 (PB.Name "d"))))), astNode 12 1 (PB.DestructuringEntry (astNode 12 1 (PB.Name "b")) (astNode 12 1 (PB.PatternBinding (astNode 12 1 (PB.Name "b")))))]),
    -- namespace matching tests
    testAstFail "test namespace patterns 1" "{,}" A.pNamespacePattern,
    testAstFail "test namespace patterns 2" "{a:}" A.pNamespacePattern,
    testAstFail "test namespace patterns 3" "{a,b,}" A.pNamespacePattern,
    testAstOk "test empty namespace pattern" "{}" A.pPattern (PB.PatternNamespace []),
    testAstOk "test single field namespace pattern" "{a}" A.pPattern (PB.PatternNamespace [astNode 2 1 (PB.DestructuringEntry (astNode 2 1 (PB.Name "a")) (astNode 2 1 (PB.PatternBinding (astNode 2 1 (PB.Name "a")))))]),
    testAstOk "test multiple namespace pattern" "{a, b}" A.pPattern (PB.PatternNamespace [astNode 2 1 (PB.DestructuringEntry (astNode 2 1 (PB.Name "a")) (astNode 2 1 (PB.PatternBinding (astNode 2 1 (PB.Name "a"))))), astNode 5 1 (PB.DestructuringEntry (astNode 5 1 (PB.Name "b")) (astNode 5 1 (PB.PatternBinding (astNode 5 1 (PB.Name "b")))))]),
    testAstOk "test multiple namespace pattern with re-assignment" "{a, b: d}" A.pPattern (PB.PatternNamespace [astNode 2 1 (PB.DestructuringEntry (astNode 2 1 (PB.Name "a")) (astNode 2 1 (PB.PatternBinding (astNode 2 1 (PB.Name "a"))))), astNode 5 4 (PB.DestructuringEntry (astNode 5 1 (PB.Name "b")) (astNode 8 1 (PB.PatternBinding (astNode 8 1 (PB.Name "d")))))]),
    -- tuple matching tests
    testAstFail "test tuple patterns 1" "()" (A.pBracketted PB.Tuple A.pPattern),
    testAstFail "test tuple patterns 2" "(a)" (A.pBracketted PB.Tuple A.pPattern),
    testAstFail "test tuple patterns 3" "(a,b,c,,)" (A.pBracketted PB.Tuple A.pPattern),
    testAstOk "test tuple patterns with no elements" "(,)" A.pPattern (PB.PatternTuple []),
    testAstOk "test tuple patterns with single element" "(a,)" A.pPattern (PB.PatternTuple [astNode 2 1 (PB.PatternBinding (astNode 2 1 (PB.Name "a")))]),
    testAstOk "test tuple patterns with multipl elements" "(a,b,_,c)" A.pPattern (PB.PatternTuple [astNode 2 1 (PB.PatternBinding (astNode 2 1 (PB.Name "a"))), astNode 4 1 (PB.PatternBinding (astNode 4 1 (PB.Name "b"))), astNode 6 1 PB.PatternIgnore, astNode 8 1 (PB.PatternBinding (astNode 8 1 (PB.Name "c")))])
  ]

-- | Test that re-assignment testing works, including some noteciable edge cases which
-- | might leak internal names.
reassignmentTests :: [Test]
reassignmentTests =
  [ testAstOk "test normal reassignment" "a = 1" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 5 1 (PB.LiteralExpr (astNode 5 1 (PB.IntLiteral 1))))),
    testAstOk "test add equals reassignment" "a += 3" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 6 (PB.FunctionCall (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "add")])))) (astNode 1 6 (funcCallArgs Nothing [astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")]))), astNode 6 1 (PB.LiteralExpr (astNode 6 1 (PB.IntLiteral 3)))]))))),
    testAstOk "test subtract equals reassignment" "a -= 9" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 6 (PB.FunctionCall (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "sub")])))) (astNode 1 6 (funcCallArgs Nothing [astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")]))), astNode 6 1 (PB.LiteralExpr (astNode 6 1 (PB.IntLiteral 9)))]))))),
    testAstOk "test multiply equals reassignment" "a *= 2" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 6 (PB.FunctionCall (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "mul")])))) (astNode 1 6 (funcCallArgs Nothing [astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")]))), astNode 6 1 (PB.LiteralExpr (astNode 6 1 (PB.IntLiteral 2)))]))))),
    testAstOk "test divide equals reassignment" "a /= 10" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 7 (PB.FunctionCall (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "div")])))) (astNode 1 7 (funcCallArgs Nothing [astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")]))), astNode 6 2 (PB.LiteralExpr (astNode 6 2 (PB.IntLiteral 10)))]))))),
    testAstOk "test logical and equals reassignment" "a &&= b" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 7 (PB.LogicalOp PB.LogicalAndOp (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 7 1 (PB.Variable (astNode 7 1 (PB.AccessName [astNode 7 1 (PB.Name "b")]))))))),
    testAstOk "test logical or equals reassignment" "a ||= b" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 7 (PB.LogicalOp PB.LogicalOrOp (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 7 1 (PB.Variable (astNode 7 1 (PB.AccessName [astNode 7 1 (PB.Name "b")]))))))),
    testAstOk "test bitwise and equals reassignment" "a &= b" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 6 (PB.FunctionCall (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "bit_and")])))) (astNode 1 6 (funcCallArgs Nothing [astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")]))), astNode 6 1 (PB.Variable (astNode 6 1 (PB.AccessName [astNode 6 1 (PB.Name "b")])))]))))),
    testAstOk "test bitwise or equals reassignment" "a |= b" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 6 (PB.FunctionCall (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "bit_or")])))) (astNode 1 6 (funcCallArgs Nothing [astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")]))), astNode 6 1 (PB.Variable (astNode 6 1 (PB.AccessName [astNode 6 1 (PB.Name "b")])))]))))),
    testAstOk "test bitwise xor equals reassignment" "a ^= b" A.pReassignment (PB.Assign (astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")])))) (astNode 1 6 (PB.FunctionCall (astNode 3 3 (PB.Variable (astNode 3 3 (PB.AccessName [astNode 3 3 (PB.Name "bit_xor")])))) (astNode 1 6 (funcCallArgs Nothing [astNode 1 2 (PB.Variable (astNode 1 2 (PB.AccessName [astNode 1 2 (PB.Name "a")]))), astNode 6 1 (PB.Variable (astNode 6 1 (PB.AccessName [astNode 6 1 (PB.Name "b")])))]))))),
    testAstFail "test unexpected reassignment operator" "a #= b" A.pReassignment,
    testAstOk "test indexing reassignment" "something[0] = 7" A.pReassignment (PB.ExprStatement (astNode 1 16 (PB.BlockExpr (astNode 1 16 (PB.Body [astNode 1 16 (PB.Let (astNode 1 16 (PB.PatternBinding $ astNode 1 16 (PB.Name "$indexed"))) Nothing Nothing (Just (astNode 1 9 (PB.Variable (astNode 1 9 (PB.AccessName [astNode 1 9 (PB.Name "something")])))))), astNode 1 16 (PB.Let (astNode 1 16 $ PB.PatternBinding $ astNode 1 16 (PB.Name "$index")) Nothing Nothing (Just (astNode 11 1 (PB.LiteralExpr (astNode 11 1 (PB.IntLiteral 0))))))] (Just (astNode 1 16 (PB.FunctionCall (astNode 1 16 (PB.Variable (astNode 1 16 (PB.AccessName [astNode 1 16 (PB.Name "index_mut")])))) (astNode 1 16 (funcCallArgs Nothing [astNode 1 16 (PB.Variable (astNode 1 16 (PB.AccessName [astNode 1 16 (PB.Name "$indexed")]))), astNode 1 16 (PB.Variable (astNode 1 16 (PB.AccessName [astNode 1 16 (PB.Name "$index")]))), astNode 16 1 (PB.LiteralExpr (astNode 16 1 (PB.IntLiteral 7)))])))))))))),
    testAstOk "test indexing function reassignment" "x[func()] = -4" A.pReassignment (PB.ExprStatement (astNode 1 14 (PB.BlockExpr (astNode 1 14 (PB.Body [astNode 1 14 (PB.Let (astNode 1 14 $ PB.PatternBinding $ astNode 1 14 (PB.Name "$indexed")) Nothing Nothing (Just (astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "x")])))))), astNode 1 14 (PB.Let (astNode 1 14 $ PB.PatternBinding $ astNode 1 14 (PB.Name "$index")) Nothing Nothing (Just (astNode 3 6 (PB.FunctionCall (astNode 3 4 (PB.Variable (astNode 3 4 (PB.AccessName [astNode 3 4 (PB.Name "func")])))) (astNode 7 2 (funcCallArgs Nothing []))))))] (Just (astNode 1 14 (PB.FunctionCall (astNode 1 14 (PB.Variable (astNode 1 14 (PB.AccessName [astNode 1 14 (PB.Name "index_mut")])))) (astNode 1 14 (funcCallArgs Nothing [astNode 1 14 (PB.Variable (astNode 1 14 (PB.AccessName [astNode 1 14 (PB.Name "$indexed")]))), astNode 1 14 (PB.Variable (astNode 1 14 (PB.AccessName [astNode 1 14 (PB.Name "$index")]))), astNode 13 2 (PB.FunctionCall (astNode 13 1 (PB.Variable (astNode 13 1 (PB.AccessName [astNode 13 1 (PB.Name "neg")])))) (astNode 14 1 (funcCallArgs Nothing [astNode 14 1 (PB.LiteralExpr (astNode 14 1 (PB.IntLiteral 4)))])))])))))))))),
    testAstOk "test property access reassignment" "person.name = x" A.pReassignment (PB.ExprStatement (astNode 1 15 (PB.BlockExpr (astNode 1 15 (PB.Body [astNode 1 15 (PB.Let (astNode 1 15 $ PB.PatternBinding $ astNode 1 15 (PB.Name "$expr")) Nothing Nothing (Just (astNode 1 6 (PB.Variable (astNode 1 6 (PB.AccessName [astNode 1 6 (PB.Name "person")])))))), astNode 1 15 (PB.Assign (astNode 1 15 (PB.PropertyAccess (astNode 1 15 (PB.Variable (astNode 1 15 (PB.AccessName [astNode 1 15 (PB.Name "$expr")])))) (astNode 8 5 (PB.Name "name")))) (astNode 15 1 (PB.Variable (astNode 15 1 (PB.AccessName [astNode 15 1 (PB.Name "x")])))))] Nothing))))),
    testAstOk "test property access reassignment 2" "person(name) = x" A.pReassignment (PB.ExprStatement (astNode 1 16 (PB.BlockExpr (astNode 1 16 (PB.Body [astNode 1 16 (PB.Let (astNode 1 16 $ PB.PatternBinding $ astNode 1 16 (PB.Name "$expr")) Nothing Nothing (Just (astNode 1 16 (PB.FunctionCall (astNode 1 6 (PB.Variable (astNode 1 6 (PB.AccessName [astNode 1 6 (PB.Name "person")])))) (astNode 7 7 (funcCallArgs Nothing [astNode 8 4 (PB.Variable (astNode 8 4 (PB.AccessName [astNode 8 4 (PB.Name "name")])))])))))), astNode 1 16 (PB.Assign (astNode 1 16 (PB.Variable (astNode 1 16 (PB.AccessName [astNode 1 16 (PB.Name "$expr")])))) (astNode 16 1 (PB.Variable (astNode 16 1 (PB.AccessName [astNode 16 1 (PB.Name "x")])))))] Nothing))))),
    testAstOk "test property access within indexing function reassignment" "y[func.name] = 8" A.pReassignment (PB.ExprStatement (astNode 1 16 (PB.BlockExpr (astNode 1 16 (PB.Body [astNode 1 16 (PB.Let (astNode 1 16 $ PB.PatternBinding $ astNode 1 16 (PB.Name "$indexed")) Nothing Nothing (Just (astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "y")])))))), astNode 1 16 (PB.Let (astNode 1 16 $ PB.PatternBinding $ astNode 1 16 (PB.Name "$index")) Nothing Nothing (Just (astNode 3 9 (PB.PropertyAccess (astNode 3 4 (PB.Variable (astNode 3 4 (PB.AccessName [astNode 3 4 (PB.Name "func")])))) (astNode 8 4 (PB.Name "name"))))))] (Just (astNode 1 16 (PB.FunctionCall (astNode 1 16 (PB.Variable (astNode 1 16 (PB.AccessName [astNode 1 16 (PB.Name "index_mut")])))) (astNode 1 16 (funcCallArgs Nothing [astNode 1 16 (PB.Variable (astNode 1 16 (PB.AccessName [astNode 1 16 (PB.Name "$indexed")]))), astNode 1 16 (PB.Variable (astNode 1 16 (PB.AccessName [astNode 1 16 (PB.Name "$index")]))), astNode 16 1 (PB.LiteralExpr (astNode 16 1 (PB.IntLiteral 8)))])))))))))),
    testAstFail "test multiple index reassignment" "y[func1(),func2()] = 0" A.pReassignment,
    testAstOk "test complex property access within indexing function add equals reassignment" "a[func()].prop[other_func(2)] += 3" A.pReassignment (PB.ExprStatement (astNode 1 34 (PB.BlockExpr (astNode 1 34 (PB.Body [astNode 1 34 (PB.Let (astNode 1 34 $ PB.PatternBinding $ astNode 1 34 (PB.Name "$indexed")) Nothing Nothing (Just (astNode 1 30 (PB.PropertyAccess (astNode 1 9 (PB.FunctionCall (astNode 1 9 (PB.Variable (astNode 1 9 (PB.AccessName [astNode 1 9 (PB.Name "index")])))) (astNode 1 9 (funcCallArgs Nothing [astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "a")]))), astNode 3 6 (PB.FunctionCall (astNode 3 4 (PB.Variable (astNode 3 4 (PB.AccessName [astNode 3 4 (PB.Name "func")])))) (astNode 7 2 (funcCallArgs Nothing [])))])))) (astNode 11 4 (PB.Name "prop")))))), astNode 1 34 (PB.Let (astNode 1 34 $ PB.PatternBinding $ astNode 1 34 (PB.Name "$index")) Nothing Nothing (Just (astNode 16 13 (PB.FunctionCall (astNode 16 10 (PB.Variable (astNode 16 10 (PB.AccessName [astNode 16 10 (PB.Name "other_func")])))) (astNode 26 3 (funcCallArgs Nothing [astNode 27 1 (PB.LiteralExpr (astNode 27 1 (PB.IntLiteral 2)))]))))))] (Just (astNode 1 34 (PB.FunctionCall (astNode 1 34 (PB.Variable (astNode 1 34 (PB.AccessName [astNode 1 34 (PB.Name "index_mut")])))) (astNode 1 34 (funcCallArgs Nothing [astNode 1 34 (PB.Variable (astNode 1 34 (PB.AccessName [astNode 1 34 (PB.Name "$indexed")]))), astNode 1 34 (PB.Variable (astNode 1 34 (PB.AccessName [astNode 1 34 (PB.Name "$index")]))), astNode 1 34 (PB.FunctionCall (astNode 31 3 (PB.Variable (astNode 31 3 (PB.AccessName [astNode 31 3 (PB.Name "add")])))) (astNode 1 34 (funcCallArgs Nothing [astNode 1 34 (PB.FunctionCall (astNode 1 34 (PB.Variable (astNode 1 34 (PB.AccessName [astNode 1 34 (PB.Name "index")])))) (astNode 1 34 (funcCallArgs Nothing [astNode 1 34 (PB.Variable (astNode 1 34 (PB.AccessName [astNode 1 34 (PB.Name "$indexed")]))), astNode 1 34 (PB.Variable (astNode 1 34 (PB.AccessName [astNode 1 34 (PB.Name "$index")])))]))), astNode 34 1 (PB.LiteralExpr (astNode 34 1 (PB.IntLiteral 3)))])))])))))))))),
    testAstOk "test nested indexing reassignment with complex property access" "b[func.name[1]].prop[other_func(x)] *= 9" A.pReassignment (PB.ExprStatement (astNode 1 40 (PB.BlockExpr (astNode 1 40 (PB.Body [astNode 1 40 (PB.Let (astNode 1 40 $ PB.PatternBinding $ astNode 1 40 (PB.Name "$indexed")) Nothing Nothing (Just (astNode 1 36 (PB.PropertyAccess (astNode 1 15 (PB.FunctionCall (astNode 1 15 (PB.Variable (astNode 1 15 (PB.AccessName [astNode 1 15 (PB.Name "index")])))) (astNode 1 15 (funcCallArgs Nothing [astNode 1 1 (PB.Variable (astNode 1 1 (PB.AccessName [astNode 1 1 (PB.Name "b")]))), astNode 3 12 (PB.FunctionCall (astNode 3 12 (PB.Variable (astNode 3 12 (PB.AccessName [astNode 3 12 (PB.Name "index")])))) (astNode 3 12 (funcCallArgs Nothing [astNode 3 12 (PB.PropertyAccess (astNode 3 4 (PB.Variable (astNode 3 4 (PB.AccessName [astNode 3 4 (PB.Name "func")])))) (astNode 8 4 (PB.Name "name"))), astNode 13 1 (PB.LiteralExpr (astNode 13 1 (PB.IntLiteral 1)))])))])))) (astNode 17 4 (PB.Name "prop")))))), astNode 1 40 (PB.Let (astNode 1 40 $ PB.PatternBinding $ astNode 1 40 (PB.Name "$index")) Nothing Nothing (Just (astNode 22 13 (PB.FunctionCall (astNode 22 10 (PB.Variable (astNode 22 10 (PB.AccessName [astNode 22 10 (PB.Name "other_func")])))) (astNode 32 3 (funcCallArgs Nothing [astNode 33 1 (PB.Variable (astNode 33 1 (PB.AccessName [astNode 33 1 (PB.Name "x")])))]))))))] (Just (astNode 1 40 (PB.FunctionCall (astNode 1 40 (PB.Variable (astNode 1 40 (PB.AccessName [astNode 1 40 (PB.Name "index_mut")])))) (astNode 1 40 (funcCallArgs Nothing [astNode 1 40 (PB.Variable (astNode 1 40 (PB.AccessName [astNode 1 40 (PB.Name "$indexed")]))), astNode 1 40 (PB.Variable (astNode 1 40 (PB.AccessName [astNode 1 40 (PB.Name "$index")]))), astNode 1 40 (PB.FunctionCall (astNode 37 3 (PB.Variable (astNode 37 3 (PB.AccessName [astNode 37 3 (PB.Name "mul")])))) (astNode 1 40 (funcCallArgs Nothing [astNode 1 40 (PB.FunctionCall (astNode 1 40 (PB.Variable (astNode 1 40 (PB.AccessName [astNode 1 40 (PB.Name "index")])))) (astNode 1 40 (funcCallArgs Nothing [astNode 1 40 (PB.Variable (astNode 1 40 (PB.AccessName [astNode 1 40 (PB.Name "$indexed")]))), astNode 1 40 (PB.Variable (astNode 1 40 (PB.AccessName [astNode 1 40 (PB.Name "$index")])))]))), astNode 40 1 (PB.LiteralExpr (astNode 40 1 (PB.IntLiteral 9)))])))]))))))))))
  ]

-- | Test that parsing break/continue/return statements works as expected.
breakReturnContinueTests :: [Test]
breakReturnContinueTests =
  [ testAstOk "test break" "break;" A.pStatement PB.Break,
    testAstFail "test break with expr" "break 3;" A.pStatement,
    testAstOk "test continue" "continue;" A.pStatement PB.Continue,
    testAstFail "test continue with expr" "continue 3;" A.pStatement,
    testAstOk "test return with expr" "return 3;" A.pStatement (PB.Return (Just (astNode 8 1 (PB.LiteralExpr (astNode 8 1 (PB.IntLiteral 3)))))),
    testAstOk "test return without expr" "return;" A.pStatement (PB.Return Nothing)
  ]

-- | Combine all the tests ready to run them
allTests :: [Test]
allTests =
  concat
    [ basicTests,
      typeTests,
      traitTests,
      literalTests,
      ifElseTests,
      enumDefTests,
      expressionTests,
      structDefTests,
      reassignmentTests,
      blockTests,
      loopWhileForTests,
      breakReturnContinueTests,
      letTests,
      patternTests
    ]
