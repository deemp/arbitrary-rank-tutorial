module Language.Arralac.Prelude.Pretty.Class where

import Language.Arralac.Prelude.Types (FastString)
import Language.LSP.Protocol.Types (UInt)
import Prettyprinter

data PrettyVerbosity
  = PrettyVerbosity'Normal
  | PrettyVerbosity'Detailed
  | PrettyVerbosity'User

type CtxPrettyVerbosity = (?prettyVerbosity :: PrettyVerbosity)

-- | Class for pretty printing.
--
-- Allows for configuring verbosity.
class Pretty' a where
  pretty' :: (CtxPrettyVerbosity) => a -> Doc ann

instance {-# OVERLAPPABLE #-} (Pretty' a) => Show a where
  show =
    let ?prettyVerbosity = PrettyVerbosity'Detailed
     in show . pretty'

instance Pretty' Int where
  pretty' = pretty

instance Pretty' Integer where
  pretty' = pretty

instance Pretty' Bool where
  pretty' = pretty

instance {-# OVERLAPPABLE #-} (Pretty' a) => Pretty' [a] where
  pretty' xs =
    encloseSep
      "[ "
      (sp <> "]")
      (line <> comma <> space)
      (pretty' <$> xs)
   where
    sp =
      case xs of
        [] -> ""
        [_] -> " "
        _ -> line

instance Pretty' String where
  pretty' = pretty

instance Pretty' FastString where
  pretty' = pretty

instance (Pretty' a) => Pretty' (Maybe a) where
  pretty' = maybe "" pretty'

instance (Pretty' a, Pretty' b) => Pretty' (a, b) where
  pretty' (a, b) = parens (pretty' a <> "," <+> pretty' b)

instance Pretty' UInt where
  pretty' = pretty' . show