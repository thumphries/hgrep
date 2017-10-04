{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep (
  -- * Parsing
    ParsedSource
  , parseModule
  -- * Searching
  , Query (..)
  , SearchResult
  , queryModule
  -- * Printing
  , PrintOpts (..)
  , defaultPrintOpts
  , ColourOpts (..)
  , printResults
  , HP.printSearchResult
  , HP.printSearchResultLocation
  ) where


import           Language.Haskell.HGrep.Internal.Data
import qualified Language.Haskell.HGrep.Print as HP
import qualified Language.Haskell.HGrep.Query as HQ
import           Language.Haskell.HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified System.IO as IO


parseModule :: FilePath -> IO (Either ParseError ParsedSource)
parseModule hs =
  bimap ParseError ParsedSource <$> EP.parseModule hs

queryModule :: Query -> ParsedSource -> [SearchResult]
queryModule q src =
  (<>)
    (HQ.findTypeDecl q src)
    (HQ.findValueDecl q src)

printResults :: PrintOpts -> [SearchResult] -> IO ()
printResults opts results =
  for_ results $ \res -> do
    IO.hPutStr IO.stdout (HP.printSearchResultLocation opts res)
    IO.hPutStrLn IO.stdout (HP.printSearchResult opts res)
