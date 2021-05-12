{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Hash standard library sources embeding. This module exports the
-- | 'standardLibrary' which is loaded in at compile time and can be
-- | referenced when resolving module imports that are from the standard
-- | library.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Embed (standardLibrary) where

import qualified Data.FileEmbed as FE
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (lift))

-- | Compile time loaded in standard library sources.
standardLibrary :: HM.HashMap FilePath Text
standardLibrary =
  HM.fromList
    [ ("prelude", $(lift $ TE.decodeUtf8 $(FE.embedFile "stdlib/prelude.hash"))),
      ("num", $(lift $ TE.decodeUtf8 $(FE.embedFile "stdlib/num.hash"))),
      ("io", $(lift $ TE.decodeUtf8 $(FE.embedFile "stdlib/io.hash"))),
      ("iter", $(lift $ TE.decodeUtf8 $(FE.embedFile "stdlib/iter.hash"))),
      ("list", $(lift $ TE.decodeUtf8 $(FE.embedFile "stdlib/list.hash"))),
      ("str", $(lift $ TE.decodeUtf8 $(FE.embedFile "stdlib/str.hash")))
    ]
