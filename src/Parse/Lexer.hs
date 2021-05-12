{-# LANGUAGE LambdaCase #-}

-- | Hash lexer definitions.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Parse.Lexer where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Error (fromMaybe)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)
import Data.Foldable (asum)
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC

-- | Typeclass used for displaying Megaparsec errors
class ShowRepr a where
  showRepr :: a -> String

instance ShowRepr Bracket where
  showRepr x = fromJust (lookup x (map swap bracketMap))

instance ShowRepr Operator where
  showRepr x = fromJust (lookup x (map swap operatorMap))

instance ShowRepr Keyword where
  showRepr x = fromJust (lookup x (map swap keywordMap))

-- | Represents an operator in Hash.
data Operator
  = Equals
  | DoubleEquals
  | TripleEquals
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | Not
  | AddEquals
  | SubtractEquals
  | MultiplyEquals
  | DivideEquals
  | ModulusEquals
  | NotEquals
  | LogicalAnd
  | LogicalOr
  | BitwiseAnd
  | VerticalBar
  | BitwiseXor
  | BitwiseNot
  | LogicalAndEquals
  | LogicalOrEquals
  | BitwiseAndEquals
  | VerticalBarEquals
  | BitwiseXorEquals
  | LessEquals
  | GreaterEquals
  | Colon
  | Period
  | Spread
  | Semicolon
  | Comma
  | Namespace
  | QuestionMark
  | Arrow
  deriving (Show, Eq)

-- | Represents a bracket.
data Bracket
  = LeftParenthesis
  | RightParenthesis
  | LeftBrace
  | RightBrace
  | LeftSquareBracket
  | RightSquareBracket
  | LeftAngleBracket
  | RightAngleBracket
  deriving (Show, Eq)

-- | Represents a keyword.
data Keyword
  = If
  | Else
  | For
  | Match
  | Let
  | In
  | As
  | Trait
  | Struct
  | Enum
  | While
  | Loop
  | Continue
  | Break
  | Import
  | Where
  | Return
  deriving (Eq)

-- | Text to 'Keyword' map, used when parsing a specific keyword,
keywordMap :: [(String, Keyword)]
keywordMap =
  [ ("if", If),
    ("else", Else),
    ("for", For),
    ("match", Match),
    ("let", Let),
    ("in", In),
    ("as", As),
    ("trait", Trait),
    ("struct", Struct),
    ("enum", Enum),
    ("while", While),
    ("loop", Loop),
    ("continue", Continue),
    ("break", Break),
    ("import", Import),
    ("where", Where),
    ("return", Return)
  ]

-- | Text to 'Operator' map, used when parsing for a specific operator
operatorMap :: [(String, Operator)]
operatorMap =
  [ ("===", TripleEquals),
    ("==", DoubleEquals),
    ("=>", Arrow),
    ("=", Equals),
    ("+=", AddEquals),
    ("-=", SubtractEquals),
    ("*=", MultiplyEquals),
    ("/=", DivideEquals),
    ("%=", ModulusEquals),
    ("!=", NotEquals),
    ("&&=", LogicalAndEquals),
    ("||=", LogicalOrEquals),
    ("&=", BitwiseAndEquals),
    ("|=", VerticalBarEquals),
    ("^=", BitwiseXorEquals),
    ("<=", LessEquals),
    (">=", GreaterEquals),
    ("::", Namespace),
    ("?", QuestionMark),
    ("+", Add),
    ("-", Subtract),
    ("*", Multiply),
    ("/", Divide),
    ("%", Modulus),
    ("!", Not),
    ("&&", LogicalAnd),
    ("||", LogicalOr),
    ("&", BitwiseAnd),
    ("|", VerticalBar),
    ("^", BitwiseXor),
    ("~", BitwiseNot),
    (":", Colon),
    ("...", Spread),
    (".", Period),
    (";", Semicolon),
    (",", Comma)
  ]

-- | Text to 'Bracket' map, used when parsing for a specific bracket
bracketMap :: [(String, Bracket)]
bracketMap =
  [ ("(", LeftParenthesis),
    (")", RightParenthesis),
    ("[", LeftSquareBracket),
    ("]", RightSquareBracket),
    ("<", LeftAngleBracket),
    (">", RightAngleBracket),
    ("{", LeftBrace),
    ("}", RightBrace)
  ]

-- | Control code map, which maps a string to a POSIX compliant control code.
controlCodes :: [(String, Char)]
controlCodes =
  [ ("a", '\a'),
    ("b", '\b'),
    ("f", '\f'),
    ("n", '\n'),
    ("r", '\r'),
    ("t", '\t'),
    ("v", '\v'),
    ("\\", '\\'),
    ("\'", '\''),
    ("\"", '\"')
  ]

instance Show Keyword where
  show k = fromMaybe (error "unexpected: show not defined for keyword") (lookup k (map swap keywordMap))

newtype Identifier = Identifier String
  deriving (Show, Eq)

-- | Represents a token in the Hash language.
data Token
  = Ident Identifier
  | Operator Operator
  | Bracket Bracket
  | Keyword Keyword
  | FloatLit Double
  | IntLit Integer
  | CharLit Char
  | StrLit String
  deriving (Show, Eq)

-- | The parser to use for lexing
type Parser m = M.ParsecT Void Text m

-- | Eat until end of line or file
eatLine :: Parser m String
eatLine = ((M.eof >> return "") <|> (MC.eol >> return "")) <|> (M.anySingle >>= \i -> (i :) <$> eatLine)

-- | Eat until a string sequence
eatUntil :: String -> Parser m String
eatUntil x = (MC.string (pack x) >> return "") <|> (M.anySingle >>= \i -> (i :) <$> eatUntil x)

-- | Eat single-line and multi-line comments
eatComment :: Parser m ()
eatComment =
  void (MC.string (pack "//") >> eatLine <* MC.space)
    <|> void (MC.string (pack "/*") >> eatUntil "*/" <* MC.space) <?> "" -- @@Cleanup: this is empty so we can ignore this during lexical error reporting

-- |
peek :: Parser m a -> Parser m Bool
peek f = (M.notFollowedBy f >> return False) <|> return True

-- | Parse an identifier
--
-- Fails on keyword.
pIdentifier :: Parser m Identifier
pIdentifier =
  pKeywordOrIdentifier >>= \case
    Left _ -> empty
    Right i -> return i

-- | Parse a keyword
-- {-# INLINE pKeyword #-}
pKeyword :: Parser m Keyword
pKeyword =
  pKeywordOrIdentifier >>= \case
    Left k -> return k
    Right _ -> empty

-- | Parse a keyword or an identifier.
pKeywordOrIdentifier :: Parser m (Either Keyword Identifier)
pKeywordOrIdentifier = do
  i <- pIdentifierLike
  case lookup i keywordMap of
    Just k -> return . Left $ k
    _ -> return . Right . Identifier $ i

-- | Parse string literal.
pStrLiteral :: Parser m String
pStrLiteral = pQuoted '\"'

-- | Parse character literal.
pCharLiteral :: Parser m Char
pCharLiteral = pQuotedSingle '\''

-- | Parse an operator.
-- {-# INLINE pOperator #-}
pOperator :: Parser m Operator
pOperator =
  pToken $
    asum [(pSymbol operatorStr <?> operatorStr) >> return operator | (operatorStr, operator) <- operatorMap]

-- | Parse a bracket.
-- {-# INLINE pBracket #-}
pBracket :: Parser m Bracket
pBracket =
  pToken $
    asum [pSymbol bracketStr >> return bracket | (bracketStr, bracket) <- bracketMap]

-- | Parse a sign (+ or -).
pSign :: Parser m Integer
pSign = (MC.char '-' >> return (-1)) <|> (M.optional (MC.char '+') >> return 1)

-- | Parse a sequence of digits, possibly separated by underscore.
pDigits :: Parser m String
pDigits = M.some $ do
  d <- M.satisfy isDigit

  -- Dissalow trailing separators in digits such as "12_"
  sep <- M.optional $ MC.char '_'
  case sep of
    Just _ -> M.lookAhead (M.satisfy isDigit) >> return d
    Nothing -> return d

-- | Integer literal parser
pIntLiteral :: Parser m Integer
pIntLiteral = pToken $ do
  digits <- pDigits
  return (read digits :: Integer)

-- | Float literal parser
pFloatLiteral :: Parser m Double
pFloatLiteral = pToken $ do
  preDigits <- pDigits
  postDigits <- M.optional . M.try $ MC.char '.' >> pDigits

  -- check for exponent
  ex <-
    M.optional . M.try $
      MC.char 'e' >> do
        exSign <- M.optional (M.try pSign)
        exDigits <- M.some $ M.satisfy isDigit
        return $ fromMaybe 1 exSign * read exDigits

  -- combine all the components together
  case (postDigits, ex) of
    (Nothing, Nothing) -> empty
    (Just p, Just e) -> let r = read (preDigits ++ "." ++ p) :: Double in return $ r * (10 ^^ e)
    (Just p, Nothing) -> return (read (preDigits ++ "." ++ p) :: Double)
    (Nothing, Just e) -> return $ (read preDigits :: Double) * (10 ^^ e)

-- | Parse a token (some combinator possibly surrounded by spaces).
-- {-# INLINE pToken #-}
pToken :: Parser m a -> Parser m a
pToken p = do
  MC.space <?> "" -- @@Cleanup: this is empty so we can ignore this during lexical error reporting
  _ <- M.many . M.try $ eatComment
  v <- p
  _ <- M.many . M.try $ eatComment
  MC.space <?> ""
  return v

-- | Parse a tokenised sequence of characters.
-- {-# INLINE pSymbol #-}
pSymbol :: String -> Parser m String
pSymbol xs = pToken (unpack <$> MC.string (pack xs))

-- | Parse an identifier character: alphanumeric or underscore.
pIdentifierChar :: Parser m Char
pIdentifierChar = M.satisfy (\next -> (isAscii next && isAlphaNum next) || next == '_')

-- | Parse a the first identifier character: alphabetic or underscore
pIdentifierfstChar :: Parser m Char
pIdentifierfstChar = M.satisfy (\next -> (isAscii next && isAlpha next) || next == '_')

-- | Parse an identifier-like character.
--
-- First character needs to be a letter or underscore. Rest of the characters
-- need to be alphanumeric or underscore.
pIdentifierLike :: Parser m String
pIdentifierLike = pToken $ do
  x <- pIdentifierfstChar
  xs <- M.many pIdentifierChar
  return (x : xs)

-- | Intrinsic parsing, prefixed by '#' some identifier.
pIntrinsicLike :: Parser m String
pIntrinsicLike = pToken $ do
  _ <- MC.char '#'
  M.some pIdentifierChar

-- | Parse a control code (i.e escape sequence).
--
-- Present in a str or char literal.
pControlCode :: Parser m Char
pControlCode = asum [MC.string (pack controlCodeStr) >> return controlCode | (controlCodeStr, controlCode) <- controlCodes]

-- | Parse a string literal, eg. "Hello world!"
pQuoted :: Char -> Parser m String
pQuoted quoteChar = pToken $ do
  _ <- MC.char quoteChar
  strInner quoteChar
  where
    quoted quoteChar = do
      q <- MC.char '\\' >> pControlCode
      rest <- strInner quoteChar
      return (q : rest)
    strInner quoteChar =
      quoted quoteChar <|> do
        c <- M.anySingle
        if c == quoteChar
          then return []
          else do
            rest <- strInner quoteChar
            return (c : rest)

-- | Parse a character literal, eg. 'A'
pQuotedSingle :: Char -> Parser m Char
pQuotedSingle quoteChar = pToken $ do
  _ <- MC.char quoteChar
  c <- charInner quoteChar
  _ <- MC.char quoteChar
  return c
  where
    charInner quoteChar =
      (MC.char '\\' >> pControlCode) <|> do
        c <- M.anySingle
        if c == quoteChar
          then empty
          else return c
