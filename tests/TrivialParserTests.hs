{-# LANGUAGE TemplateHaskell #-}

-- | Hash Compiler types and primitives for type checking.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module TrivialParserTests where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (State, runState)
import Data.Text (Text, pack)
import Data.Void (Void)
import Debug.Trace
import GHC.IO.Encoding
import qualified Parse.Ast as A
import qualified Parse.Lexer as L
import System.IO
import Test.QuickCheck
import Test.QuickCheck.All
import TestUtils
import qualified Text.Megaparsec as M
import Text.Read

extractToken :: Maybe [L.Token] -> Maybe Integer
extractToken (Just [L.IntLit a]) = Just a
extractToken _ = Nothing

extractFToken :: Maybe [L.Token] -> Maybe Double
extractFToken (Just [L.FloatLit a]) = Just a
extractFToken _ = Nothing

extractCToken :: Maybe [L.Token] -> Maybe Char
extractCToken (Just [L.CharLit a]) = Just a
extractCToken _ = Nothing

extractSToken :: Maybe [L.Token] -> Maybe String
extractSToken (Just [L.StrLit a]) = Just a
extractSToken _ = Nothing

runParser :: String -> Maybe [L.Token]
runParser d = case runParserWithState lexer A.emptyModuleContext d of
  (Left _) -> Nothing
  (Right ts) -> Just ts

newtype IntString = IntString String deriving (Show)

instance Arbitrary IntString where
  arbitrary = listOf (elements "123456790") >>= \x -> return $ IntString x

newtype FloatString = FloatString String deriving (Show)

instance Arbitrary FloatString where
  arbitrary = do
    start <- listOf1 $ elements "1234567890"
    end <- listOf $ elements "e1234567890"

    return $ FloatString (concat [start, ".", end])

charString :: Gen String
charString = arbitrary

strString :: Gen String
strString = arbitrary

prop_testInt :: IntString -> Bool
prop_testInt (IntString inp) = extractToken (runParser inp) == (readMaybe inp :: Maybe Integer)

prop_testChar :: String -> Bool
prop_testChar inp = extractCToken (runParser inp) == (readMaybe inp :: Maybe Char)

prop_testStr :: String -> Bool
prop_testStr inp = extractSToken (runParser inp) == (readMaybe inp :: Maybe String)

test1 :: IO ()
test1 = quickCheck prop_testInt

-- Haskell float parsing is evidently different to the way Hash parses floats.
-- test2 :: IO ()
-- test2 = quickCheck $ prop_testFloat

test3 :: IO ()
test3 = quickCheck $ forAll charString $ const prop_testChar

test4 :: IO ()
test4 = quickCheck $ forAll strString $ const prop_testStr

return []

runTests = $quickCheckAll
