{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynTermVar.Rn where

import Language.Arralac.Pass.Types
import Language.Arralac.Syntax.TTG.SynTermVar
import Language.Arralac.Type.Local.RnVar

type instance XSynTerm'Var CompRn = RnVar