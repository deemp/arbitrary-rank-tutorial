module Language.Arralac.Typecheck.Bag where

import Data.List (intersperse)
import Language.Arralac.Utils.Pretty (Pretty' (..))
import Prettyprinter (indent, vsep)

-- | A list of something.
--
-- Similar to @Bag@ in GHC.
-- However, GHC uses a tree-like structure for the `Bag`.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/Bag.hs#L48
newtype Bag a = Bag {bag :: [a]}
  deriving newtype (Functor, Semigroup, Monoid)

emptyBag :: Bag a
emptyBag = Bag []

unitBag :: a -> Bag a
unitBag x = Bag [x]

unionBags :: Bag a -> Bag a -> Bag a
unionBags b1 b2 = b1 <> b2

consBag :: a -> Bag a -> Bag a
consBag el b = unitBag el `unionBags` b

instance (Pretty' a) => Pretty' (Bag a) where
  pretty' (Bag []) = "Bag []"
  pretty' (Bag xs) =
    vsep
      [ "Bag"
      , "["
      , indent 2 (vsep (intersperse "," (pretty' <$> xs)))
      , "]"
      ]
