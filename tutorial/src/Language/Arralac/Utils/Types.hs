module Language.Arralac.Utils.Types where

import Data.Text (Text)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/FastString.hs#L211
type FastString = Text

-- ==============================================
-- [Implicit parameters]
-- ==============================================

-- | Current file path.
type ICurrentFilePath = (?currentFilePath :: FastString)

-- | Whether to output debug logs.
type IDebug = (?debug :: Bool)