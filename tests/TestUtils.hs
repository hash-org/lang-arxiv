-- | Hash Compiler types and primitives for type checking.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module TestUtils where

import Control.Error (catMaybes)
import Control.Monad.State (State, runState)
import Data.Functor ((<&>))
import Data.Text
import Data.Void
import qualified Parse.Boot as PB
import qualified Parse.Lexer as L
import qualified Text.Megaparsec as M

-- | Lex a character stream into a stream of tokens.
lexer :: L.Parser m [L.Token]
lexer = do
  c <- catMaybes <$> M.many pAny
  M.eof -- make sure we parsed everything
  return c

-- | Internal lexer testing parser
pAny :: L.Parser m (Maybe L.Token)
pAny =
  (L.eatComment >> return Nothing) -- No token if we eat comment
    M.<|> Just -- Otherwise token
      <$> ( L.Operator <$> L.pOperator
              M.<|> ( L.pKeywordOrIdentifier <&> \case
                        Left k -> L.Keyword k
                        Right i -> L.Ident i
                    )
              M.<|> M.try (L.FloatLit <$> L.pFloatLiteral)
              M.<|> M.try (L.IntLit <$> L.pIntLiteral)
              M.<|> L.Ident <$> L.pIdentifier
              M.<|> L.Bracket <$> L.pBracket
              M.<|> L.StrLit <$> L.pStrLiteral
              M.<|> L.CharLit <$> L.pCharLiteral
          )

-- | Function that runs a parser with an empty context.
runParserWithState :: L.Parser (State c) a -> c -> String -> Either (M.ParseErrorBundle Text Void) a
runParserWithState parser initState inp = let (res, _) = runState (M.runParserT parser "" (pack inp)) initState in res
