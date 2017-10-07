{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep (
  -- * Parsing
    ParsedSource
  , parseModule
  , ParseError
  , HP.printParseError
  -- * Searching
  -- ** Queries
  , Query (..)
  , Regex
  , compileRegex
  -- ** Running queries
  , SearchResult
  , queryModule
  -- * Printing results
  , PrintOpts (..)
  , defaultPrintOpts
  , ColourOpts (..)
  , printResults
  , HP.printSearchResult
  , HP.printSearchResultLocation
  ) where


import           Language.Haskell.HGrep.Internal.Data
import           Language.Haskell.HGrep.Internal.Print
import           Language.Haskell.HGrep.Prelude
import qualified Language.Haskell.HGrep.Print as HP
import qualified Language.Haskell.HGrep.Query as HQ

import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified System.IO as IO

import qualified SrcLoc


parseModule :: FilePath -> IO (Either ParseError ParsedSource)
parseModule hs =
  bimap ParseError ParsedSource <$> EP.parseModule hs

queryModule :: Query -> ParsedSource -> [SearchResult]
queryModule q src =
  (<>)
    (HQ.findTypeDecl q src)
    (HQ.findValueDecl q src)

printResults :: PrintOpts -> [SearchResult] -> IO ()
printResults opts results = do
  let printedResult = fmap (printWithLocation opts) results
  for_ (foldAdjacent printedResult) $ \(textResult, span) -> do
    IO.hPutStr   IO.stdout (printSrcSpan opts span)
    IO.hPutStrLn IO.stdout textResult

type TextWithLocation = ([Char], SrcLoc.SrcSpan)

printWithLocation :: PrintOpts -> SearchResult -> TextWithLocation
printWithLocation opts result@(SearchResult _ loc) =
  (HP.printSearchResult opts result, SrcLoc.getLoc loc)

foldAdjacent :: [TextWithLocation] -> [TextWithLocation]
foldAdjacent []                              = []
foldAdjacent [result]                        = [result]
foldAdjacent (firstResult:secondResult:rest) =
  case (firstResult, secondResult) of
    ((firstText, firstSpan), (secondText, secondSpan)) ->
      case mergedLocs firstSpan secondSpan of
        Nothing   ->
          firstResult : foldAdjacent (secondResult:rest)
        Just span ->
          foldAdjacent $ (firstText <> secondText, span) : rest

mergedLocs :: SrcLoc.SrcSpan -> SrcLoc.SrcSpan -> Maybe SrcLoc.SrcSpan
mergedLocs span1 span2
  | areAdjacentSpans span1 span2 = Just $ SrcLoc.combineSrcSpans span1 span2
  | otherwise                    = Nothing

areAdjacentSpans :: SrcLoc.SrcSpan -> SrcLoc.SrcSpan -> Bool
areAdjacentSpans span1 span2 =
  areAdjacentLocs (SrcLoc.srcSpanEnd span1) (SrcLoc.srcSpanStart span2)

areAdjacentLocs :: SrcLoc.SrcLoc -> SrcLoc.SrcLoc -> Bool
areAdjacentLocs (SrcLoc.RealSrcLoc loc1) (SrcLoc.RealSrcLoc loc2) =
  SrcLoc.srcLocLine loc1 + 1 == SrcLoc.srcLocLine loc2
areAdjacentLocs _ _ = False
