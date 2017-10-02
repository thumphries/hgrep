{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep where


import           Language.Haskell.HGrep.Internal.Data
import qualified Language.Haskell.HGrep.Print as HP
import qualified Language.Haskell.HGrep.Query as HQ
import           Language.Haskell.HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified System.IO as IO


grepFile :: FilePath -> [Char] -> IO ()
grepFile hs name =
  void . runEitherT $ do
    modl <- parseModule hs
    liftIO $ printSearchResults (HQ.findTypeDecl name modl)
    liftIO $ printSearchResults (HQ.findValueDecl name modl)
    pure ()

parseModule :: FilePath -> EitherT ParseError IO ParsedSource
parseModule hs =
  bimapT ParseError ParsedSource . EitherT $ EP.parseModule hs

printSearchResults :: [SearchResult] -> IO ()
printSearchResults =
  traverse_ (IO.putStrLn . HP.printSearchResult)
