{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.Extension.Rn where

import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.TTG.Extension
import Language.Arralac.Utils.Pass

type instance XSynType'Var' CompRn = ()
type instance XSynType'ForAll' CompRn = SrcSpan
type instance XSynType'Fun' CompRn = SrcSpan
type instance XSynType'Concrete' CompRn = ()

type instance XSynTerm'Var' CompRn = ()
type instance XSynTerm'Lit' CompRn = SrcSpan
type instance XSynTerm'App' CompRn = SrcSpan
type instance XSynTerm'Lam' CompRn = SrcSpan
type instance XSynTerm'ALam' CompRn = SrcSpan
type instance XSynTerm'Let' CompRn = SrcSpan
type instance XSynTerm'Ann' CompRn = SrcSpan
