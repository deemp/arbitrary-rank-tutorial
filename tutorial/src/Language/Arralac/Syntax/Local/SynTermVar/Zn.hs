{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Syntax.Local.SynTermVar.Zn where

import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.TyVar.Zn ()
import Language.Arralac.Syntax.Local.Type ()
import Language.Arralac.Syntax.TTG.SynTermVar
import Language.Arralac.Syntax.TTG.Type (Type)
import Prettyprinter

type instance XSynTerm'Var CompZn = ZnTermVar

data ZnTermVar
  = -- | Variable identifier
    -- Always local and vanilla.
    ZnTermVar
    { varName :: !Name
    , varType :: Type CompZn
    }

instance Pretty' ZnTermVar where
  pretty' ZnTermVar{varName, varType} =
    parens (pretty' varName <+> "::" <+> pretty' varType)
