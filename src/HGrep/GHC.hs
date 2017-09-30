{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HGrep.GHC where


import qualified Data.List as L
import           Data.Maybe (catMaybes)

import           HGrep.Data
import           HGrep.Prelude

import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified FastString
import qualified HsSyn
import qualified OccName
import qualified Outputable
import qualified RdrName
import qualified SrcLoc



findTypeDecl :: [Char] -> ParsedSource -> [Char]
findTypeDecl name (ParsedSource (anns, locMod)) =
  let
    modl = SrcLoc.unLoc locMod
    decls = HsSyn.hsmodDecls modl
    print ast = EP.exactPrint ast anns
  in
    L.unlines . fmap print . catMaybes . with decls $ \ldec ->
      sequenceA . with ldec $ \dec ->
        case dec of
          HsSyn.TyClD d ->
            case d of
              HsSyn.FamDecl fam ->
                empty
              HsSyn.SynDecl n tvs rhs fvs ->
                empty
              HsSyn.DataDecl n tvs rhs cusk fvs ->
                case SrcLoc.unLoc n of
                  RdrName.Unqual ocn ->
                    if fastEq name (OccName.occNameFS ocn)
                      then pure dec
                      else empty
                  RdrName.Qual _ ocn ->
                    if fastEq name (OccName.occNameFS ocn)
                      then pure dec
                      else empty
                  _ ->
                    empty
              HsSyn.ClassDecl ctx n tvs fds sigs meths ats atds docs fvs ->
                empty
          _ ->
            empty

unOccName :: OccName.OccName -> [Char]
unOccName ocn =
  FastString.unpackFS (OccName.occNameFS ocn)

fastEq :: [Char] -> FastString.FastString -> Bool
fastEq s fs =
  FastString.mkFastString s == fs
