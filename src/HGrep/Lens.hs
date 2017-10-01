{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module HGrep.Lens where


import           Control.Lens

import           HGrep.Lens.Rules (makeOptics)
import           HGrep.Prelude

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

makeOptics ''HsBindLR

makeOptics ''HsExpr
makeOptics ''SyntaxExpr
makeOptics ''MatchGroup
makeOptics ''StmtLR

makeOptics ''HsLit

makeOptics ''HsType
