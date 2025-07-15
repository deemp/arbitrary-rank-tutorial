{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Type.Local.TyVar.Rn where

import Language.Arralac.Pass.Types
import Language.Arralac.Type.Local.RnVar
import Language.Arralac.Type.TTG.TyVar

type instance XTyVar CompRn = RnVar