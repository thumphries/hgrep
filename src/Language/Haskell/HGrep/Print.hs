{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep.Print (
    printParseError
  , printSearchResult
  , printSearchResultLocation
  ) where


import qualified Data.List as L

import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour

import           Language.Haskell.HGrep.Internal.Data
import           Language.Haskell.HGrep.Prelude

import qualified System.Console.ANSI as ANSI

import qualified Outputable
import qualified SrcLoc


printParseError :: ParseError -> [Char]
printParseError (ParseError (loc, msg)) =
  L.intercalate ": " [
      unsafePpr loc
    , msg
    ]

printSearchResult :: PrintOpts -> SearchResult -> [Char]
printSearchResult (PrintOpts co) (SearchResult anns ast) =
  let src = EP.exactPrint ast anns in
    case co of
      DefaultColours ->
        hscolour src
      NoColours ->
        src

printSearchResultLocation :: PrintOpts -> SearchResult -> [Char]
printSearchResultLocation (PrintOpts co) (SearchResult _anns ast) =
  let loc = chomp (unsafePpr (SrcLoc.getLoc ast)) in
    case co of
      DefaultColours ->
        ansiLocationFormat <> loc <> ansiReset
      NoColours ->
        loc

ansiLocationFormat :: [Char]
ansiLocationFormat =
  ANSI.setSGRCode [
      ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
    , ANSI.SetUnderlining ANSI.SingleUnderline
    ]

ansiReset :: [Char]
ansiReset =
  ANSI.setSGRCode []

hscolour :: [Char] -> [Char]
hscolour =
  HsColour.hscolour HsColour.TTY HsColour.defaultColourPrefs False False "" False

unsafePpr :: Outputable.Outputable o => o -> [Char]
unsafePpr =
  Outputable.showSDocUnsafe . Outputable.ppr

chomp :: [Char] -> [Char]
chomp =
  L.unlines . L.lines
