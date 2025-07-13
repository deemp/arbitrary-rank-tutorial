{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.Extension.Tc where

import Language.Arralac.Prelude.Pass
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.Extension

type instance XSynType'Var' CompTc = ()
type instance XSynType'ForAll' CompTc = SrcSpan
type instance XSynType'Fun' CompTc = SrcSpan
type instance XSynType'Concrete' CompTc = ()

type instance XSynTerm'Var' CompTc = ()
type instance XSynTerm'Lit' CompTc = TcAnno
type instance XSynTerm'App' CompTc = TcAnno
type instance XSynTerm'Lam' CompTc = TcAnno
type instance XSynTerm'ALam' CompTc = TcAnno
type instance XSynTerm'Let' CompTc = TcAnno
type instance XSynTerm'Ann' CompTc = TcAnno

data TcAnno = TcAnno
  { annoSrcLoc :: SrcSpan
  , annoType :: Expected TcType
  }
