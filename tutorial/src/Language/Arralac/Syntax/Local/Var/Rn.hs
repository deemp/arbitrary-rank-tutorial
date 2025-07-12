{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.Var.Rn where

import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.TTG.Type
import Language.Arralac.Utils.Pass
import Language.Arralac.Utils.Pretty

type instance XVar' CompRn = RnVar

-- | A term or a type variable produced by the renamer.
--
-- Similar to @RdrName@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Reader.hs#L166
data RnVar = RnVar
  { varName :: !Name
  }

instance Eq RnVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord RnVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Pretty' RnVar where
  pretty' var = pretty' var.varName