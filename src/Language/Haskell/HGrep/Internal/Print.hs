{-# LANGUAGE NoImplicitPrelude #-}

-- | Internal private functions for 'Language.Haskell.HGrep.Print'.

module Language.Haskell.HGrep.Internal.Print (
    printSrcSpan
  , unsafePpr
  ) where

import qualified Data.List as L

import           Language.Haskell.HGrep.Internal.Data
import           Language.Haskell.HGrep.Prelude

import qualified Outputable
import qualified System.Console.ANSI as ANSI
import qualified SrcLoc

printSrcSpan :: PrintOpts -> SrcLoc.SrcSpan -> [Char]
printSrcSpan (PrintOpts co _) span =
  let loc = chomp (unsafePpr span) in
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

unsafePpr :: Outputable.Outputable o => o -> [Char]
unsafePpr =
  Outputable.showSDocUnsafe . Outputable.ppr

chomp :: [Char] -> [Char]
chomp =
  L.unlines . L.lines
