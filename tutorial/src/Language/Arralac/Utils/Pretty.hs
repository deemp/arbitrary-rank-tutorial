module Language.Arralac.Utils.Pretty (
  module Class,
  module GPretty,
  prettyCompact,
  prettyDetailed,
  prettyUser,
  prettyIndent,
  parensIndent,
  vsep',
) where

import Language.Arralac.Utils.Pretty.Class as Class
import Language.Arralac.Utils.Pretty.GPretty as GPretty
import Prettyprinter

prettyCompact :: (Pretty' a) => a -> Doc ann
prettyCompact = let ?prettyVerbosity = PrettyVerbosity'Normal in pretty'

prettyDetailed :: (Pretty' a) => a -> Doc ann
prettyDetailed = let ?prettyVerbosity = PrettyVerbosity'Detailed in pretty'

prettyUser :: (Pretty' a) => a -> Doc ann
prettyUser = let ?prettyVerbosity = PrettyVerbosity'User in pretty'

prettyIndent :: (CtxPrettyVerbosity, Pretty' a) => a -> Doc ann
prettyIndent = indent 2 . pretty'

parensIndent :: Doc ann -> Doc ann
parensIndent x = parens (line <> indent 2 x <> line)

vsep' :: [Doc ann] -> Doc ann
vsep' xs = encloseSep "" "" line xs <> line