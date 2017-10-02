{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep.Print where


import qualified Data.List as L

import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour

import           Language.Haskell.HGrep.Internal.Data
import           Language.Haskell.HGrep.Prelude

import qualified Outputable
import qualified SrcLoc


printSearchResult :: SearchResult -> [Char]
printSearchResult (SearchResult anns ast) =
  L.concat [
      unsafePpr (SrcLoc.getLoc ast)
    , hscolour (EP.exactPrint ast anns)
    ]

hscolour :: [Char] -> [Char]
hscolour =
  HsColour.hscolour HsColour.TTY HsColour.defaultColourPrefs False False "" False

unsafePpr :: Outputable.Outputable o => o -> [Char]
unsafePpr =
  Outputable.showSDocUnsafe . Outputable.ppr
