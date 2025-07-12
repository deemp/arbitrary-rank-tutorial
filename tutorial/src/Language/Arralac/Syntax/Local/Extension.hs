{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.Extension where

import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.Extension
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Utils.Pass

type instance XSynTerm'AnnoCommon CompRn = SrcSpan
type instance XSynTerm'AnnoCommon CompTc = TcAnno
type instance XSynTerm'AnnoCommon CompZn = ZnAnno

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

data TcAnno = TcAnno
  { annoSrcLoc :: SrcSpan
  , annoType :: Expected TcType
  }

data ZnAnno = ZnAnno
  { annoSrcLoc :: SrcSpan
  , annoType :: ZnType
  }
