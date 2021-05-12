{-# LANGUAGE ViewPatterns #-}

-- | Hash Compiler errors for type checking.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Typecheck.Error where

import Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text
import Data.Void
import qualified Parse.Boot as PB
import Text.Megaparsec as M

-- | Create a typechecking error with the given information.
makeError :: FilePath -> String -> Text -> PB.AstNode a -> ParseErrorBundle Text Void
makeError path msg input (PB.offsetBegin -> offset) = do
  let initialState =
        PosState
          { pstateInput = input,
            pstateOffset = 0,
            pstateSourcePos = initialPos path,
            pstateTabWidth = defaultTabWidth,
            pstateLinePrefix = ""
          }

  -- @Improvement: we could provide the ast token here?
  let errorBundle =
        ParseErrorBundle
          { bundleErrors = NonEmpty.fromList [FancyError offset $ Set.singleton (ErrorFail msg)],
            -- A collection of 'ParseError's that is sorted by parse error offsets
            bundlePosState = initialState -- State that is used for line\/column calculation
          }

  errorBundle

-- | Create a type error using the given `ParseErrorBundle`.
constructTypeError :: ParseErrorBundle Text Void -> String
constructTypeError bn = "Failed to type check:\n" ++ errorBundlePretty bn
