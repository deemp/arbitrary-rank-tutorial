module Language.Arralac.Prelude.Pretty (
  module Class,
  module GPretty,
  prettyCompact,
  prettyDetailed,
  prettyUser,
  prettyIndent,
  parensIndent,
  vsep',
  defaultPrettyWidth,
) where

import Language.Arralac.Prelude.Pretty.Class as Class
import Language.Arralac.Prelude.Pretty.GPretty as GPretty
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
vsep' = foldMap (\x -> x <> line)

defaultPrettyWidth :: Int
defaultPrettyWidth = 200