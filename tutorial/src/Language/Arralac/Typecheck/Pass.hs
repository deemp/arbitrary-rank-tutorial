module Language.Arralac.Typecheck.Pass where

import Data.Data (Data)

-- ==============================================
-- [Compiler pass]
-- ==============================================
-- We don't implement a compiler (yet),
-- but still follow the naming used in GHC.

-- | Pass of the compiler.
--
-- Similar to 'CompPass in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L169
data Pass = Renamed | Typechecked | Zonked
  deriving stock (Data)

-- | Used as a data type index for the 'SynTerm', 'SynType', 'Type'.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L157
data CompPass (c :: Pass) where
  CompRn :: CompPass 'Renamed
  CompTc :: CompPass 'Typechecked
  CompZn :: CompPass 'Zonked

-- Type synonyms as a shorthand for tagging
--
-- Similar to 'GhcPs' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L173

-- | Output of renamer
type CompRn = CompPass 'Renamed

-- | Output of typechecker
type CompTc = CompPass 'Typechecked

-- | Output of zonker.
--
-- Doesn't contain metavariables by construction.
type CompZn = CompPass 'Zonked
