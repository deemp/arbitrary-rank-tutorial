{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynTypeConcrete where

import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty.Class
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Type.TTG.Concrete

type instance XSynType'Concrete CompRn = Concrete
type instance XSynType'Concrete CompTc = Concrete
type instance XSynType'Concrete CompZn = Concrete

-- | Concrete type.
data Concrete = Concrete
  { concreteName :: Name
  , concreteType :: TyConcrete
  }

instance Eq Concrete where
  c1 == c2 = c1.concreteType == c2.concreteType

instance Pretty' Concrete where
  pretty' c = pretty' c.concreteType
