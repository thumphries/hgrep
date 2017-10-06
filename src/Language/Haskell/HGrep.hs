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
import qualified Language.Haskell.HGrep.Print as HP
import qualified Language.Haskell.HGrep.Query as HQ
import           Language.Haskell.HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint as EP

import           System.Exit (ExitCode (..))
import qualified System.IO as IO
import qualified Control.Exception as E

parseModule :: FilePath -> IO (Either ParseError ParsedSource)
parseModule hs = do
  res <- E.tryJust handler (bimap ExactPrintParseError ParsedSource <$> EP.parseModule hs)
  return $ case res of
    Left e -> Left e
    Right v -> v
    where handler :: ExitCode -> Maybe ParseError
          handler (ExitFailure _) = Just ExactPrintException
          handler _ = Nothing

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
