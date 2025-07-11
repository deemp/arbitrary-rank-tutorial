{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.Extension where

import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.TTG.Extension
import Language.Arralac.Syntax.TTG.SynTerm

type instance XSynType'Var' x = ()
type instance XSynType'ForAll' x = SrcSpan
type instance XSynType'Fun' x = SrcSpan
type instance XSynType'Concrete' x = ()

type instance XSynTerm'Var' x = ()
type instance XSynTerm'Lit' x = XSynTerm'AnnoCommon x
type instance XSynTerm'App' x = XSynTerm'AnnoCommon x
type instance XSynTerm'Lam' x = XSynTerm'AnnoCommon x
type instance XSynTerm'ALam' x = XSynTerm'AnnoCommon x
type instance XSynTerm'Let' x = XSynTerm'AnnoCommon x
type instance XSynTerm'Ann' x = XSynTerm'AnnoCommon x