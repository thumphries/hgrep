{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HGrep.GHC where


import           Control.Lens

import qualified Data.List as L
import           Data.Maybe (catMaybes)
import           Data.Monoid (First)

import           HGrep.Data
import           HGrep.Lens
import           HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour

import qualified FastString
import qualified OccName
import qualified RdrName


findTypeDecl :: [Char] -> ParsedSource -> [Char]
findTypeDecl name (ParsedSource (anns, locMod)) =
  let
    decls = locMod ^. _unloc . _hsmodDecls
    print ast = hscolour (EP.exactPrint ast anns)
  in
    L.unlines . fmap print . catMaybes . with decls $ \ldec ->
      sequenceA . with ldec $ \dec -> do
        n <- match dec [
            _TyClD . _DataDecl . _1 . _unloc
          , _TyClD . _SynDecl  . _1 . _unloc
          ]
        guard (compareName name n)
        pure dec

findValueDecl :: [Char] -> ParsedSource -> [Char]
findValueDecl name (ParsedSource (anns, locMod)) =
  let
    decls = locMod ^. _unloc . _hsmodDecls
    print ast = hscolour (EP.exactPrint ast anns)
  in
    L.unlines . fmap print . catMaybes . with decls $ \ldec ->
      sequenceA . with ldec $ \dec -> do
        n <- match dec [
            _ValD . _FunBind . _1 . _unloc
          , _ValD . _VarBind . _1
-- FIXME list of names here
--          , _SigD . _TypeSig . _1
          ]
        guard (compareName name n)
        pure dec

compareName :: [Char] -> RdrName.RdrName -> Bool
compareName name n =
  case n of
    RdrName.Unqual ocn ->
      fastEq name (OccName.occNameFS ocn)
    RdrName.Qual _ ocn ->
      fastEq name (OccName.occNameFS ocn)
    _ ->
      False


unOccName :: OccName.OccName -> [Char]
unOccName ocn =
  FastString.unpackFS (OccName.occNameFS ocn)

fastEq :: [Char] -> FastString.FastString -> Bool
fastEq s fs =
  FastString.mkFastString s == fs

match :: Foldable t => s -> t (Getting (First a) s a) -> Maybe a
match a ps =
  preview (fold ps) a

hscolour :: [Char] -> [Char]
hscolour =
  HsColour.hscolour HsColour.TTY HsColour.defaultColourPrefs False False "" False
