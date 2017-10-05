{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep.Print (
    printParseError
  , printSearchResult
  , printSearchResultLocation
  ) where


import qualified Data.List as L
import qualified Data.Map  as Map

import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Types as EP
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
  -- Get the start position of the comment before search result
  let annsPairs      = Map.toList anns
      targetAnnPairs = L.filter (isSameLoc .fst) annsPairs
      resLoc         = getSpanStartLine resSpan
      startLineNum   =
        case targetAnnPairs of
          []             -> resLoc
          ((_, ann) : _) ->
            case EP.annPriorComments ann of
              []                 -> resLoc
              ((comment, _) : _) -> getSpanStartLine $ EP.commentIdentifier comment in
  -- ignore empty lines before the actual result
  let wholeSrc    = EP.exactPrint ast anns
      (nill, src) = L.span null $ L.lines wholeSrc in
    case co of
      DefaultColours ->
        L.unlines $ nill <> L.zipWith printLine [startLineNum..] src
      NoColours ->
        wholeSrc
  where
    resSpan :: SrcLoc.SrcSpan
    resSpan = SrcLoc.getLoc ast

    isSameLoc :: EP.AnnKey -> Bool
    isSameLoc (EP.AnnKey loc _) = loc == resSpan

    printLine :: Int -> [Char] -> [Char]
    printLine i l = show i <> "  " <> hscolour l

    getSpanStartLine :: SrcLoc.SrcSpan -> Int
    getSpanStartLine someSpan =
      case SrcLoc.srcSpanStart someSpan of
        SrcLoc.RealSrcLoc x -> SrcLoc.srcLocLine x
        -- TODO: Don't know how to get rid of error
        _                   -> error "SrcLoc is FastString"

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
