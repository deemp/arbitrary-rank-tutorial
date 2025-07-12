module Language.Arralac.Utils.Unique.Types where

import Language.Arralac.Utils.Pretty.Class (Pretty')

-- | A globally unique identifier.
--
-- Similar to @Unique@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique.hs#L98
newtype Unique = Unique {unique :: Int}
  deriving newtype (Eq, Ord, Pretty')
