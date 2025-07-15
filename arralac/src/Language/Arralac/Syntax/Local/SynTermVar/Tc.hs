{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Syntax.Local.SynTermVar.Tc where

import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.TTG.SynTermVar
import Language.Arralac.Type.Local.TyVar.Tc ()
import Language.Arralac.Type.TTG.Concrete ()
import Language.Arralac.Type.Local.Concrete ()
import Language.Arralac.Type.Local.Type
import Prettyprinter

type instance XSynTerm'Var CompTc = TcTermVar

-- | A term variable, possibly with a known type.
data TcTermVar
  = TcTermVar
  { varName :: !Name
  , varType :: Expected TcType
  }

instance Pretty' TcTermVar where
  pretty' var =
    hsep
      [ pretty' var.varName
      , "::"
      , braces (pretty' var.varType)
      ]
