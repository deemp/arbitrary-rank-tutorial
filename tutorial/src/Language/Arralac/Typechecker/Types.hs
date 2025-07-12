module Language.Arralac.Typechecker.Types where

import Language.Arralac.Utils.Pretty
import Prettyprinter

-- | Similar to @TcLevel@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L698
--
-- Also see Note [TcLevel invariants] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L705
newtype TcLevel = TcLevel Int
  deriving newtype (Show, Eq, Ord, Num)

type CtxTcLevel = (?tcLevel :: TcLevel)

instance Pretty' TcLevel where
  pretty' (TcLevel lvl) = "L" <+> pretty' lvl
