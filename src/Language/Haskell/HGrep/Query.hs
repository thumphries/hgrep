{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Language.Haskell.HGrep.Query (
    findTypeDecl
  , findValueDecl
  ) where


import           Control.Lens

import           Data.Foldable (any)
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Monoid (First)

import           Language.Haskell.HGrep.Internal.Data
import           Language.Haskell.HGrep.Internal.Lens
import           Language.Haskell.HGrep.Prelude

import           Text.Regex.PCRE.Heavy ((=~))

import qualified FastString
import qualified HsDecls
import qualified OccName
import qualified RdrName
import           SrcLoc (unLoc)
#if MIN_VERSION_base(4,11,0)
import qualified GHC
#endif


findTypeDecl :: Query -> ParsedSource -> [SearchResult]
findTypeDecl q src =
  matchDecls src $ \decl ->
    fromMaybe False . match decl $
#if !MIN_VERSION_base(4,12,0)
         _TyClD . _DataDecl . _1 . _unloc . to (nameQuery q)
      <> _TyClD . _SynDecl . _1 . _unloc . to (nameQuery q)
#else
         _TyClD . _2 . _DataDecl . _2 . _unloc . to (nameQuery q)
      <> _TyClD . _2 . _SynDecl . _2 . _unloc . to (nameQuery q)
#endif

findValueDecl :: Query -> ParsedSource -> [SearchResult]
findValueDecl q src =
  matchDecls src $ \decl ->
    fromMaybe False . match decl $
#if !MIN_VERSION_base(4,12,0)
         _ValD . _FunBind . _1 . _unloc . to (nameQuery q)
      <> _ValD . _VarBind . _1 . to (nameQuery q)
      <> _SigD . _TypeSig . _1 . to (any (nameQuery q . unLoc))
#else
         _ValD . _2 . _FunBind . _2 . _unloc . to (nameQuery q)
      <> _ValD . _2 . _VarBind . _2 . to (nameQuery q)
      <> _SigD . _2 . _TypeSig . _2 . to (any (nameQuery q . unLoc))
#endif

#if !MIN_VERSION_base(4,11,0)
matchDecls :: ParsedSource -> (HsDecls.HsDecl RdrName.RdrName -> Bool) -> [SearchResult]
#else
matchDecls :: ParsedSource -> (HsDecls.HsDecl GHC.GhcPs -> Bool) -> [SearchResult]
#endif
matchDecls (ParsedSource (anns, locMod)) p =
  fmap (SearchResult anns) $
    L.filter (p . unLoc) (locMod ^. _unloc . _hsmodDecls)

nameQuery :: Query -> RdrName.RdrName -> Bool
nameQuery q n =
  case q of
    MatchSimple name ->
      compareName name n
    MatchRegex (Regex rex) ->
      nameToString n =~ rex

compareName :: [Char] -> RdrName.RdrName -> Bool
compareName name n =
  case n of
    RdrName.Unqual ocn ->
      fastEq name (OccName.occNameFS ocn)
    RdrName.Qual _ ocn ->
      fastEq name (OccName.occNameFS ocn)
    _ ->
      False

nameToFS :: RdrName.RdrName -> FastString.FastString
nameToFS n =
  OccName.occNameFS $
    case n of
      RdrName.Unqual ocn ->
        ocn
      RdrName.Qual _mod ocn ->
        ocn
      RdrName.Orig _mod ocn ->
        ocn
      RdrName.Exact name ->
        name ^. _n_occ

nameToString :: RdrName.RdrName -> [Char]
nameToString =
  FastString.unpackFS . nameToFS

fastEq :: [Char] -> FastString.FastString -> Bool
fastEq s fs =
  FastString.mkFastString s == fs

match :: s -> Getting (First a) s a -> Maybe a
match =
  flip preview
{-# INLINE match #-}
