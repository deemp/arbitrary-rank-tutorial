module Language.Arralac.Type.TTG.Concrete where

import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types (FastString)

-- | A concrete type.
data TyConcrete
  = TyConcrete'Int
  | TyConcrete'Bool
  | TyConcrete'String
  | -- | Type with a single constructor.
    --
    -- That constructor has the same name as the type.
    TyConcrete'Con FastString
  deriving stock (Eq)

-- TODO parameterize?

tyConcreteName :: TyConcrete -> FastString
tyConcreteName = \case
  TyConcrete'Int -> "Int"
  TyConcrete'Bool -> "Bool"
  TyConcrete'String -> "String"
  TyConcrete'Con name -> name

instance Pretty' TyConcrete where
  pretty' = pretty' . tyConcreteName
