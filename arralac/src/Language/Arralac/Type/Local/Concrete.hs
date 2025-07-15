{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Type.Local.Concrete where

import Language.Arralac.Pass.Types
import Language.Arralac.Type.TTG.Concrete
import Language.Arralac.Type.TTG.Type

type instance XTyConcrete CompRn = TyConcrete
type instance XTyConcrete CompTc = TyConcrete
type instance XTyConcrete CompZn = TyConcrete
