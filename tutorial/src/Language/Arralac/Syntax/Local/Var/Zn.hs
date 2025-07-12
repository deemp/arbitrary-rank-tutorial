{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.Var.Zn where

import Language.Arralac.Syntax.Local.Name (Name)
import Language.Arralac.Syntax.Local.Type ()
import Language.Arralac.Syntax.TTG.Type (Type, XVar')
import Language.Arralac.Utils.Pass (CompZn)
import Language.Arralac.Utils.Pretty
import Prettyprinter

type instance XVar' CompZn = ZnTyVar

-- | A zonked type variable.
--
-- Doesn't have a type, unlike 'Id' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L277
data ZnTyVar
  = -- | Variable identifier
    -- Always local and vanilla.
    ZnTyVar
    { varName :: !Name
    }

data ZnTermVar
  = -- | Variable identifier
    -- Always local and vanilla.
    ZnTermVar
    { varName :: !Name
    , varType :: Type CompZn
    }

instance Eq ZnTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord ZnTyVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Pretty' ZnTyVar where
  pretty' ZnTyVar{varName} = pretty' varName

instance Pretty' ZnTermVar where
  pretty' ZnTermVar{varName, varType} =
    parens (pretty' varName <+> "::" <+> pretty' varType)