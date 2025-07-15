{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Type.Local.RnVar where

import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Type.TTG.TyVar

type instance XTyVar CompRn = RnVar

-- | A term or a type variable produced by the renamer.
--
-- Similar to @RdrName@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Reader.hs#L166
newtype RnVar = RnVar
  { varName :: Name
  }
  deriving newtype (Eq, Ord, Pretty')
