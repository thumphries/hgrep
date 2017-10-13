{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep.Print (
    printParseError
  , printSearchResult
  , printSearchResultLocation
  ) where


import           Data.Char (isSpace)
import qualified Data.List as L
import qualified Data.Map as Map

import qualified Text.Printf as T

import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Types as EP
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour

import           Language.Haskell.HGrep.Internal.Data
import           Language.Haskell.HGrep.Internal.Print
import           Language.Haskell.HGrep.Prelude

import qualified SrcLoc


printParseError :: ParseError -> [Char]
printParseError ExactPrintException = "Parsing failed unexpectedly with an exception"
printParseError (ExactPrintParseError (loc, msg)) =
  L.intercalate ": " [
      unsafePpr loc
    , msg
    ]

printSearchResult :: PrintOpts -> SearchResult -> [Char]
printSearchResult (PrintOpts co lno) (SearchResult anns ast) =
  -- Get the start position of the comment before search result
  case lno of
        PrintLineNums -> numberedSrc
        NoLineNums    -> nonNumberedSrc
  where
    colorize :: [Char] -> [Char]
    colorize anySrc =
      case co of
        DefaultColours -> hscolour anySrc
        NoColours      -> anySrc

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

    nonEmptySrc :: [Char]
    (_, nonEmptySrc) = fmap colorize $ L.span isSpace wholeSrc

    nonNumberedSrc = nonEmptySrc

    -- Doesn't prepent locations when there is no start line number
    printWithLineNums :: Maybe Int -> [Char]
    printWithLineNums Nothing      = nonNumberedSrc
    printWithLineNums (Just start) =
      L.unlines $ L.zipWith prependLineNum [start..] (L.lines nonEmptySrc)

    annsPairs      = Map.toList anns
    targetAnnPairs = L.filter (isSameLoc .fst) annsPairs
    resLoc         = getSpanStartLine resSpan
    startLineNum   =
        case targetAnnPairs of
          []             -> resLoc
          ((_, ann) : _) ->
            case EP.annPriorComments ann of
              []                 -> resLoc
              ((comment, _) : _) -> getSpanStartLine $ EP.commentIdentifier comment

    numberedSrc = printWithLineNums startLineNum

    -- Adds line numbers at the start of each line
    prependLineNum :: Int -> [Char] -> [Char]
    prependLineNum i l = T.printf "%5d" i <> " ┃ " <> l

printSearchResultLocation :: PrintOpts -> SearchResult -> [Char]
printSearchResultLocation opts (SearchResult _anns ast) =
  printSrcSpan opts (SrcLoc.getLoc ast)

hscolour :: [Char] -> [Char]
hscolour =
  HsColour.hscolour HsColour.TTY HsColour.defaultColourPrefs False False "" False
