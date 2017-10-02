{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HGrep.Lens.Rules where


import           Control.Lens

import           Language.Haskell.HGrep.Prelude

import           Language.Haskell.TH (Name, DecsQ, mkName, nameBase)


rules :: LensRules
rules =
  lensRules
    & set' lensField namer
    & set' simpleLenses False
    & set' createClass False
    & set' generateSignatures True

namer :: FieldNamer
namer _tn _fields field =
  [TopName (mkName ("_" <> nameBase field))]

makeOptics :: Name -> DecsQ
makeOptics tn =
  (<>)
    <$> makeLensesWith rules tn
    <*> makePrisms tn
