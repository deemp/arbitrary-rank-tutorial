module Language.Arralac.Prelude.Types where

import Data.Text (Text)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/FastString.hs#L211
type FastString = Text

type FastFilePath = Text

-- ==============================================
-- [Implicit parameters]
-- ==============================================

-- | Current file path.
type CtxInputFilePath = (?inputFilePath :: FastFilePath)

-- | Whether to output debug logs.
type CtxDebug = (?debug :: Bool)
