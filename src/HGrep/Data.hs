{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module HGrep.Data where


import qualified GHC
import qualified SrcLoc

import           HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint.Annotater as EA
import qualified Language.Haskell.GHC.ExactPrint.Types as ET

import qualified SrcLoc


newtype ParsedSource = ParsedSource {
    unParsedSource :: (ET.Anns, GHC.Located (GHC.HsModule GHC.RdrName))
  }

newtype ParseError = ParseError {
    unParseError :: (SrcLoc.SrcSpan, [Char])
  }

data SearchResult =
  forall ast. EA.Annotate ast =>
    SearchResult ET.Anns (SrcLoc.Located ast)
