{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.HGrep.Query (
    findTypeDecl
  , findValueDecl
  , printSearchResult
  ) where


import           Control.Lens

import           Data.Foldable (any)
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Monoid (First)

import           Language.Haskell.HGrep.Internal.Data
import           Language.Haskell.HGrep.Internal.Lens
import           Language.Haskell.HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour

import qualified FastString
import qualified HsDecls
import qualified OccName
import qualified Outputable
import qualified RdrName
import           SrcLoc (unLoc)


findTypeDecl :: [Char] -> ParsedSource -> [SearchResult]
findTypeDecl name src =
  matchDecls src $ \decl ->
    fromMaybe False . match decl $
         _TyClD . _DataDecl . _1 . _unloc . to (compareName name)
      <> _TyClD . _SynDecl . _1 . _unloc . to (compareName name)

findValueDecl :: [Char] -> ParsedSource -> [SearchResult]
findValueDecl name src =
  matchDecls src $ \decl ->
    fromMaybe False . match decl $
         _ValD . _FunBind . _1 . _unloc . to (compareName name)
      <> _ValD . _VarBind . _1 . to (compareName name)
      <> _SigD . _TypeSig . _1 . to (any (compareName name . unLoc))

matchDecls :: ParsedSource -> (HsDecls.HsDecl RdrName.RdrName -> Bool) -> [SearchResult]
matchDecls (ParsedSource (anns, locMod)) p =
  fmap (SearchResult anns) $
    L.filter (p . unLoc) (locMod ^. _unloc . _hsmodDecls)

compareName :: [Char] -> RdrName.RdrName -> Bool
compareName name n =
  case n of
    RdrName.Unqual ocn ->
      fastEq name (OccName.occNameFS ocn)
    RdrName.Qual _ ocn ->
      fastEq name (OccName.occNameFS ocn)
    _ ->
      False

printSearchResult :: SearchResult -> [Char]
printSearchResult (SearchResult anns ast) =
  L.concat [
      unsafePpr (ast ^. _loc)
    , hscolour (EP.exactPrint ast anns)
    ]

fastEq :: [Char] -> FastString.FastString -> Bool
fastEq s fs =
  FastString.mkFastString s == fs

match :: s -> Getting (First a) s a -> Maybe a
match =
  flip preview
{-# INLINE match #-}

hscolour :: [Char] -> [Char]
hscolour =
  HsColour.hscolour HsColour.TTY HsColour.defaultColourPrefs False False "" False

unsafePpr :: Outputable.Outputable o => o -> [Char]
unsafePpr =
  Outputable.showSDocUnsafe . Outputable.ppr
