module TestLexer where

import Control.Error (catMaybes)
import Control.Monad.State (State, runState)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Void (Void)
import qualified Parse.Ast as A
import qualified Parse.Lexer as L
import Test.HUnit (Test (TestCase), assertEqual, assertFailure)
import TestUtils (lexer, runParserWithState)
import qualified Text.Megaparsec as M

-- | Function that expects a parser to return OK and some list of tokens.
testLexerOk :: String -> String -> [L.Token] -> Test
testLexerOk desc inp out = TestCase (assertEqual desc (Right out) (runParserWithState lexer (A.emptyModuleContext "") inp))

-- | Function that expects a parser to fail.
testLexerFail :: String -> String -> Test
testLexerFail desc inp =
  TestCase
    ( case runParserWithState lexer (A.emptyModuleContext "") inp of
        Right a -> assertFailure (msg a)
        Left a -> return ()
    )
  where
    msg a =
      (if null desc then "" else desc ++ "\n")
        ++ "expected failure but got: "
        ++ show a

-- | First ever written test! Just check if an empty input returns 0 tokens.
basicTests :: [Test]
basicTests =
  [ testLexerOk "empty input yields no tokens" "" []
  ]

-- | Test identifier keyword parsing
identifierTests :: [Test]
identifierTests =
  [ testLexerOk "test identifier (1)" "hello" [L.Ident (L.Identifier "hello")],
    testLexerOk "test identifier (2)" "_whateve_r_" [L.Ident (L.Identifier "_whateve_r_")],
    testLexerFail "test identifier starts with number" "12_whatever",
    testLexerOk "test identifier contains number" "wh12atever" [L.Ident (L.Identifier "wh12atever")],
    testLexerFail "test identifier non-ascii" "helloγεια"
  ]

-- | Test numerical literal parsing
numberLiteralTests :: [Test]
numberLiteralTests =
  [ testLexerOk "test number literal" "1" [L.IntLit 1],
    testLexerOk "test number literal decimal" "3.14" [L.FloatLit 3.14],
    testLexerOk "test number literal exponent decimal" "2.5e98" [L.FloatLit 2.5000000000000008e98],
    testLexerOk "test number literal float exponent" "2e2.2" [L.FloatLit 2e2, L.Operator L.Period, L.IntLit 2],
    testLexerFail "test number trailing separator (1)" "12_",
    testLexerFail "test number trailing separator (2)" "123_1_",
    testLexerOk "test number literal negative exponent decimal" "-0.1e-1" [L.Operator L.Subtract, L.FloatLit 1.0000000000000002e-2],
    testLexerOk "test number literal trailing period (2)" "1." [L.IntLit 1, L.Operator L.Period],
    testLexerOk "test number literal decimal separators (2)" "1_000_000" [L.IntLit 1000000],
    testLexerOk "test number literal lone exponent" "e8" [L.Ident (L.Identifier "e8")]
  ]

-- | Test string and character literal parsing
strCharLiteralTests :: [Test]
strCharLiteralTests =
  [ testLexerOk "test string literal basic" "\"Hello, world!\"" [L.StrLit "Hello, world!"],
    testLexerOk "test string literal unicode" "\"Γειά σου!\"" [L.StrLit "Γειά σου!"],
    testLexerOk "test string literal empty" "\"\"" [L.StrLit ""],
    testLexerOk "test string literal single" "\"A\"" [L.StrLit "A"],
    testLexerOk "test string literal escape quotes" "\"String containing \\\"double quotes\\\"\"" [L.StrLit "String containing \"double quotes\""],
    testLexerOk "test string literal escape slash" "\"Escape backslash \\\\. \"" [L.StrLit "Escape backslash \\. "],
    testLexerOk "test string literal escape control" "\"Unix whitespace: \\t\\r\\n\\v\\f. \"" [L.StrLit "Unix whitespace: \t\r\n\v\f. "],
    testLexerFail "test string literal unmatched" "\"This is\" unmatched\"",
    testLexerOk "test char literal" "\'A\'" [L.CharLit 'A'],
    testLexerOk "test char literal unicode" "\'ά\'" [L.CharLit 'ά'],
    testLexerFail "test char literal empty" "\'\'",
    testLexerOk "test char literal escape quote" "\'\\\'\'" [L.CharLit '\''],
    testLexerOk "test char literal escape slash" "\'\\\\\'" [L.CharLit '\\'],
    testLexerOk "test char literal escape control" "\'\\n\'" [L.CharLit '\n']
  ]

-- | Test that comments are thrown out during parsing
commentTests :: [Test]
commentTests =
  [ testLexerOk "test single comment ignored" "// This is a comment" [],
    testLexerOk "test multi comment ignored" "/* This is a comment */" [],
    testLexerOk "test multi comment with code" "let x = /* This is a comment */5;" [L.Keyword L.Let, L.Ident (L.Identifier "x"), L.Operator L.Equals, L.IntLit 5, L.Operator L.Semicolon],
    testLexerOk "test single comment with code" "// Define pi\nlet pi = 3.14;" [L.Keyword L.Let, L.Ident (L.Identifier "pi"), L.Operator L.Equals, L.FloatLit 3.14, L.Operator L.Semicolon],
    testLexerOk "test multi comment ignored" "/* This is a comment which spans\n* multi-lines and has some \t\ttabs and spaces      */let a = 5;" [L.Keyword L.Let, L.Ident (L.Identifier "a"), L.Operator L.Equals, L.IntLit 5, L.Operator L.Semicolon]
  ]

-- general operator tests
operatorTests :: [Test]
operatorTests =
  [ testLexerOk "test not operator" "!a" [L.Operator L.Not, L.Ident (L.Identifier "a")],
    testLexerOk "test B-AND operator" "a       &    b" [L.Ident (L.Identifier "a"), L.Operator L.BitwiseAnd, L.Ident (L.Identifier "b")],
    testLexerOk "test B-AND operator" "a | b" [L.Ident (L.Identifier "a"), L.Operator L.VerticalBar, L.Ident (L.Identifier "b")]
  ]

-- check parse precedence with operators
precedenceTests :: [Test]
precedenceTests =
  [ testLexerOk "test greater than precedence" "=>" [L.Operator L.Arrow],
    testLexerOk "test greater than precedence" ">=" [L.Operator L.GreaterEquals],
    testLexerOk "test less than precedence" "<=" [L.Operator L.LessEquals],
    testLexerOk "test double equals precedence" "==" [L.Operator L.DoubleEquals],
    testLexerOk "test add-equals precedence" "+=" [L.Operator L.AddEquals],
    testLexerOk "test add-equals precedence" "+=" [L.Operator L.AddEquals],
    testLexerOk "test sub-equals precedence" "-=" [L.Operator L.SubtractEquals],
    testLexerOk "test mul-equals precedence" "*=" [L.Operator L.MultiplyEquals],
    testLexerOk "test div-equals precedence" "/=" [L.Operator L.DivideEquals],
    testLexerOk "test bxor-equals precedence (1)" "a ^= b" [L.Ident (L.Identifier "a"), L.Operator L.BitwiseXorEquals, L.Ident (L.Identifier "b")],
    testLexerOk "test bxor-equals precedence (2)" "a ^^= b" [L.Ident (L.Identifier "a"), L.Operator L.BitwiseXor, L.Operator L.BitwiseXorEquals, L.Ident (L.Identifier "b")],
    testLexerOk "test triple-equals precedence" "===" [L.Operator L.TripleEquals],
    testLexerOk "test B-OR precedence (1)" "|=" [L.Operator L.VerticalBarEquals],
    testLexerOk "test B-OR precedence (1)" "| =" [L.Operator L.VerticalBar, L.Operator L.Equals],
    testLexerOk "test L-OR precedence (2)" "||" [L.Operator L.LogicalOr],
    testLexerOk "test L-OR precedence (3)" "||=" [L.Operator L.LogicalOrEquals],
    testLexerOk "test B-AND precedence (1)" "&=" [L.Operator L.BitwiseAndEquals],
    testLexerOk "test L-AND precedence (2)" "&&" [L.Operator L.LogicalAnd],
    testLexerOk "test L-AND precedence (3)" "&&=" [L.Operator L.LogicalAndEquals],
    testLexerOk "test L-SPREAD precedence (2)" "... . .." [L.Operator L.Spread, L.Operator L.Period, L.Operator L.Period, L.Operator L.Period],
    testLexerOk "test NAMESPACE precedence" ":: : : ::" [L.Operator L.Namespace, L.Operator L.Colon, L.Operator L.Colon, L.Operator L.Namespace],
    -- ensure that identifiers that begin as prefixed keywords don't get parsed as keywords first
    testLexerOk "test IF identifier precedence" "ife" [L.Ident (L.Identifier "ife")],
    testLexerOk "test ELSE identifier precedence" "elsea" [L.Ident (L.Identifier "elsea")],
    testLexerOk "test FOR identifier precedence" "fort" [L.Ident (L.Identifier "fort")],
    testLexerOk "test MATCH identifier precedence" "matches" [L.Ident (L.Identifier "matches")],
    testLexerOk "test LET identifier precedence" "lettuce" [L.Ident (L.Identifier "lettuce")],
    testLexerOk "test IN identifier precedence" "inn" [L.Ident (L.Identifier "inn")],
    testLexerOk "test TRAIT identifier precedence" "traits" [L.Ident (L.Identifier "traits")],
    testLexerOk "test STRUCT identifier precedence" "structure" [L.Ident (L.Identifier "structure")],
    testLexerOk "test ENUM identifier precedence" "enumerator" [L.Ident (L.Identifier "enumerator")],
    testLexerOk "test WHILE identifier precedence" "whilesey" [L.Ident (L.Identifier "whilesey")],
    testLexerOk "test LOOP identifier precedence" "loops" [L.Ident (L.Identifier "loops")],
    testLexerOk "test CONTINUE identifier precedence" "continue_" [L.Ident (L.Identifier "continue_")],
    testLexerOk "test BREAK identifier precedence" "breaker" [L.Ident (L.Identifier "breaker")],
    testLexerOk "test IMPORT identifier precedence" "imports" [L.Ident (L.Identifier "imports")],
    testLexerOk "test WHERE identifier precedence" "where_am_i" [L.Ident (L.Identifier "where_am_i")],
    testLexerOk "test RETURN identifier precedence" "return_to_sender" [L.Ident (L.Identifier "return_to_sender")]
  ]

-- | Test bracket parsing
bracketTests :: [Test]
bracketTests =
  [ testLexerOk "test bracket recognition" ")(][}{><" [L.Bracket L.RightParenthesis, L.Bracket L.LeftParenthesis, L.Bracket L.RightSquareBracket, L.Bracket L.LeftSquareBracket, L.Bracket L.RightBrace, L.Bracket L.LeftBrace, L.Bracket L.RightAngleBracket, L.Bracket L.LeftAngleBracket],
    testLexerOk "test angle brackets in expression" "<A, B>" [L.Bracket L.LeftAngleBracket, L.Ident (L.Identifier "A"), L.Operator L.Comma, L.Ident (L.Identifier "B"), L.Bracket L.RightAngleBracket],
    testLexerOk "test parenthesees in expression" "(A, B)" [L.Bracket L.LeftParenthesis, L.Ident (L.Identifier "A"), L.Operator L.Comma, L.Ident (L.Identifier "B"), L.Bracket L.RightParenthesis],
    testLexerOk "test braces in expression (1)" "{A: B}" [L.Bracket L.LeftBrace, L.Ident (L.Identifier "A"), L.Operator L.Colon, L.Ident (L.Identifier "B"), L.Bracket L.RightBrace],
    testLexerOk "test braces in expression (2)" "{A, B}" [L.Bracket L.LeftBrace, L.Ident (L.Identifier "A"), L.Operator L.Comma, L.Ident (L.Identifier "B"), L.Bracket L.RightBrace]
  ]

-- | Combine all the tests ready to run them
allTests :: [Test]
allTests = concat [basicTests, identifierTests, numberLiteralTests, strCharLiteralTests, commentTests, operatorTests, precedenceTests, bracketTests]
