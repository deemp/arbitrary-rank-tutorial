{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.Extension.Zn where

import Language.Arralac.Pass.Types
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.TTG.Extension
import Language.Arralac.Type.Local.Type

type instance XSynType'Var' CompZn = ()
type instance XSynType'ForAll' CompZn = SrcSpan
type instance XSynType'Fun' CompZn = SrcSpan
type instance XSynType'Concrete' CompZn = ()

type instance XSynTerm'Var' CompZn = ()
type instance XSynTerm'Lit' CompZn = ZnAnno
type instance XSynTerm'App' CompZn = ZnAnno
type instance XSynTerm'Lam' CompZn = ZnAnno
type instance XSynTerm'ALam' CompZn = ZnAnno
type instance XSynTerm'Let' CompZn = ZnAnno
type instance XSynTerm'Ann' CompZn = ZnAnno

data ZnAnno = ZnAnno
  { annoSrcLoc :: SrcSpan
  , annoType :: ZnType
  }
