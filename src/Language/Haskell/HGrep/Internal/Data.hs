{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.HGrep.Internal.Data (
    ParsedSource (..)
  , ParseError (..)
  , Query
  , SearchResult (..)
  , PrintOpts (..)
  , defaultPrintOpts
  , ColourOpts (..)
  ) where


import           Language.Haskell.HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint.Annotater as EA
import qualified Language.Haskell.GHC.ExactPrint.Types as ET

import qualified GHC
import qualified SrcLoc


newtype ParsedSource = ParsedSource {
    unParsedSource :: (ET.Anns, GHC.Located (GHC.HsModule GHC.RdrName))
  }

newtype ParseError = ParseError {
    unParseError :: (SrcLoc.SrcSpan, [Char])
  }

type Query = [Char]

data SearchResult =
  forall ast. EA.Annotate ast =>
    SearchResult ET.Anns (SrcLoc.Located ast)

data PrintOpts = PrintOpts {
    poColourOpts :: ColourOpts
  } deriving (Eq, Ord, Show)

data ColourOpts =
    DefaultColours
  | NoColours
  deriving (Eq, Ord, Show)

defaultPrintOpts :: PrintOpts
defaultPrintOpts =
  PrintOpts {
      poColourOpts = DefaultColours
    }
