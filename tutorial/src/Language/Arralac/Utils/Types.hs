module Language.Arralac.Utils.Types where

import Data.IORef (IORef)
import Data.Text

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/FastString.hs#L211
type FastString = Text

-- ==============================================
-- [Implicit parameters]
-- ==============================================

-- | A global counter used for creating globally unique names.
--
-- GHC implements a much more sophisticated unique supply.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique/Supply.hs#L67
type IUniqueSupply = (?uniqueSupply :: IORef Int)

-- | Current file path.
type ICurrentFilePath = (?currentFilePath :: FastString)

-- | Whether to output debug logs.
type IDebug = (?debug :: Bool)