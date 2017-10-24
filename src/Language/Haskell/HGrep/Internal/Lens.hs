{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Haskell.HGrep.Internal.Lens where


import           Control.Lens

import           Language.Haskell.HGrep.Internal.Lens.Rules (makeOptics)
import           Language.Haskell.HGrep.Prelude

import           HsBinds
import           HsDecls
import           HsExpr
import           HsLit
import           HsSyn
import           OccName
import           RdrName
import           SrcLoc


makeOptics ''GenLocated

_loc :: Lens' (Located e) SrcSpan
_loc = _L . _1

_unloc :: Lens' (Located e) e
_unloc = _L . _2

makeOptics ''OccName
makeOptics ''RdrName

makeOptics ''HsModule

makeOptics ''HsDecl
makeOptics ''TyClDecl
makeOptics ''InstDecl
makeOptics ''DerivDecl
makeOptics ''Sig
makeOptics ''DefaultDecl
makeOptics ''ForeignDecl
makeOptics ''WarnDecls
makeOptics ''AnnDecl
makeOptics ''RuleDecls
makeOptics ''VectDecl
makeOptics ''SpliceDecl
makeOptics ''DocDecl
makeOptics ''RoleAnnotDecl

makeOptics ''ImportDecl
makeOptics ''IE

makeOptics ''HsDataDefn
makeOptics ''ConDecl
makeOptics ''HsImplicitBndrs
makeOptics ''HsConDetails

makeOptics ''HsBindLR

makeOptics ''HsExpr
makeOptics ''SyntaxExpr
makeOptics ''MatchGroup
makeOptics ''StmtLR

makeOptics ''HsLit

makeOptics ''HsType
makeOptics ''ConDeclField
makeOptics ''FieldOcc
