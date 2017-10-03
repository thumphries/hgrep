{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Language.Haskell.HGrep.Query (
    findTypeDecl
  , findTypeUses
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

import qualified FastString
import qualified HsDecls
import qualified HsTypes
import qualified OccName
import qualified RdrName
import           SrcLoc (unLoc)


findTypeDecl :: [Char] -> ParsedSource -> [SearchResult]
findTypeDecl name src =
  matchDecls src $
       _TyClD . _DataDecl . _1 . _unloc . to (compareName name)
    <> _TyClD . _SynDecl . _1 . _unloc . to (compareName name)

findValueDecl :: [Char] -> ParsedSource -> [SearchResult]
findValueDecl name src =
  matchDecls src $
       _ValD . _FunBind . _1 . _unloc . to (compareName name)
    <> _ValD . _VarBind . _1 . to (compareName name)
    <> _SigD . _TypeSig . _1 . to (any (compareName name . unLoc))

findTypeUses :: [Char] -> ParsedSource -> [SearchResult]
findTypeUses name src =
  matchDecls src $
      _TyClD . _DataDecl . _3 . _HsDataDefn . _5 . folded
        -- FIXME losing sum types here somewhere
        . _unloc . _ConDeclH98 . _4 . _PrefixCon . folded
        . _unloc . to (typeContains name)

typeContains :: [Char] -> HsTypes.HsType Name -> Bool
typeContains name ty =
  fromMaybe False . flip preview ty $
       _HsForAllTy . _2 . folded . to (typeContains name)
    <> _HsQualTy . _2 . folded . to (typeContains name)
    <> _HsTyVar . _unloc . to (compareName name)

-- -----------------------------------------------------------------------------

type Name = RdrName.RdrName
type Declaration = HsDecls.HsDecl Name

matchDecls :: ParsedSource -> Getting (First Bool) Declaration Bool -> [SearchResult]
matchDecls src patterns =
  matchDecls' src $ \decl ->
    fromMaybe False (preview patterns decl)

matchDecls' :: ParsedSource -> (HsDecls.HsDecl RdrName.RdrName -> Bool) -> [SearchResult]
matchDecls' (ParsedSource (anns, locMod)) p =
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

fastEq :: [Char] -> FastString.FastString -> Bool
fastEq s fs =
  FastString.mkFastString s == fs
