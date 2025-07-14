module Language.Arralac.Prelude.Unique.Supply where

import Data.IORef (IORef, readIORef, writeIORef)
import Language.Arralac.Prelude.Unique.Types (Unique (Unique))

-- | A global counter used for creating globally unique names.
--
-- GHC implements a much more sophisticated unique supply.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique/Supply.hs#L67
type CtxUniqueSupply = (?uniqueSupply :: IORef Int)

-- | Generate a new unique id.
--
-- Similar to @genSym@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique/Supply.hs#L257
newUnique' :: (CtxUniqueSupply) => IO Int
newUnique' = do
  r <- readIORef ?uniqueSupply
  writeIORef ?uniqueSupply (r + 1)
  pure r

-- | Generate a new 'Unique'.
--
-- Similar to @uniqFromTag@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique/Supply.hs#L282
newUnique :: (CtxUniqueSupply) => IO Unique
newUnique = Unique <$> newUnique'
