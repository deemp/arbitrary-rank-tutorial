{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Arralac.Syntax.Local.SynLit where

import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Syntax.TTG.Lit

type instance XSynTerm'Lit CompRn = SynLit
type instance XSynTerm'Lit CompTc = SynLit
type instance XSynTerm'Lit CompZn = SynLit

-- | A literal.
--
-- Its constructors don't have extension points
-- because 'SynLit' is wrapped into 'SynType'Concrete' that does.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Lit.hs#L48
data SynLit
  = SynLit'Num Integer
  | SynLit'Bool Bool
  | SynLit'Str FastString
  | SynLit'Con FastString

-- TODO add extension fields?

instance Pretty' SynLit where
  pretty' = \case
    SynLit'Num val -> pretty' val
    SynLit'Str val -> "\"" <> pretty' val <> "\""
    SynLit'Bool val -> pretty' val
    SynLit'Con val -> pretty' val
