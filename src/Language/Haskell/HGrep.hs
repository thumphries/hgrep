{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep where


import           Language.Haskell.HGrep.Data
import qualified Language.Haskell.HGrep.GHC as HG
import           Language.Haskell.HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified System.IO as IO


grepFile :: FilePath -> [Char] -> IO ()
grepFile hs name =
  void . runEitherT $ do
    modl <- parseModule hs
    liftIO $ printSearchResults (HG.findTypeDecl name modl)
    liftIO $ printSearchResults (HG.findValueDecl name modl)
    pure ()

parseModule :: FilePath -> EitherT ParseError IO ParsedSource
parseModule hs =
  bimapT ParseError ParsedSource . EitherT $ EP.parseModule hs

printSearchResults :: [SearchResult] -> IO ()
printSearchResults =
  traverse_ (IO.putStrLn . HG.printSearchResult)
