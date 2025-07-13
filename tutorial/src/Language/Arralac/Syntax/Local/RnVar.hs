{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.RnVar where

import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.TTG.SynTermVar
import Language.Arralac.Syntax.TTG.TyVar
import Language.Arralac.Utils.Pass
import Language.Arralac.Utils.Pretty

type instance XTyVar CompRn = RnVar
type instance XSynTerm'Var CompRn = RnVar

-- | A term or a type variable produced by the renamer.
--
-- Similar to @RdrName@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Reader.hs#L166
newtype RnVar = RnVar
  { varName :: Name
  }
  deriving newtype (Eq, Ord, Pretty')
