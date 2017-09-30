{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HGrep where


import           HGrep.Data
import qualified HGrep.GHC as HG
import           HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified System.IO as IO


grepFile :: FilePath -> [Char] -> IO ()
grepFile hs name =
  void . runEitherT $ do
    modl <- parseModule hs
    liftIO $ IO.putStrLn (HG.findTypeDecl name modl)
    pure ()

parseModule :: FilePath -> EitherT ParseError IO ParsedSource
parseModule hs =
  bimapT ParseError ParsedSource . EitherT $ EP.parseModule hs
