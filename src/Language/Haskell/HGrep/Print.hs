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
printSearchResult (PrintOpts co lno) (SearchResult anns ast) =
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
              ((comment, _) : _) -> getSpanStartLine $ EP.commentIdentifier comment
      numberedSrc = printWithLineNums startLineNum in
    colorize $ case lno of
      PrintLineNums -> numberedSrc
      NoLineNums    -> wholeSrc
  where
    colorize :: [Char] -> [Char]
    colorize anySrc =
      case co of
        DefaultColours ->
          hscolour anySrc
        NoColours ->
          anySrc

    resSpan :: SrcLoc.SrcSpan
    resSpan = SrcLoc.getLoc ast

    isSameLoc :: EP.AnnKey -> Bool
    isSameLoc (EP.AnnKey loc _) = loc == resSpan

    -- Returns the line number if possible to find
    getSpanStartLine :: SrcLoc.SrcSpan -> Maybe Int
    getSpanStartLine someSpan =
      case SrcLoc.srcSpanStart someSpan of
        SrcLoc.RealSrcLoc x -> Just $ SrcLoc.srcLocLine x
        _                   -> Nothing

    -- ignore empty lines before the actual result
    wholeSrc :: [Char]
    wholeSrc = EP.exactPrint ast anns

    nill, src :: [[Char]]
    (nill, src) = L.span null $ L.lines wholeSrc

    -- Doesn't prepent locations when there is no start line number
    printWithLineNums :: Maybe Int -> [Char]
    printWithLineNums Nothing      = wholeSrc
    printWithLineNums (Just start) =
      L.unlines $ nill <> L.zipWith prependLineNum [start..] src

    -- Adds line numbers at the start of each line
    prependLineNum :: Int -> [Char] -> [Char]
    prependLineNum i l = show i <> "  " <> l

printSearchResultLocation :: PrintOpts -> SearchResult -> [Char]
printSearchResultLocation (PrintOpts co _) (SearchResult _anns ast) =
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
