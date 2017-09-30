{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HGrep.Data where


import qualified GHC
import qualified SrcLoc

import           HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint.Types as ET


newtype ParsedSource = ParsedSource {
    unParsedSource :: (ET.Anns, GHC.Located (GHC.HsModule GHC.RdrName))
  }

newtype ParseError = ParseError {
    unParseError :: (SrcLoc.SrcSpan, [Char])
  }
