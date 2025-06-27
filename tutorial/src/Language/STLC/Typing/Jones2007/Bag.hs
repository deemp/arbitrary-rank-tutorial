{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.STLC.Typing.Jones2007.Bag where

-- GHC uses a tree-like structure for the `Bag`.
-- A list will be enough in our case.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/Bag.hs#L48
newtype Bag a = Bag {bag :: [a]}
  deriving newtype (Functor, Semigroup)

emptyBag :: Bag a
emptyBag = Bag []

unitBag :: a -> Bag a
unitBag x = Bag [x]

unionBags :: Bag a -> Bag a -> Bag a
unionBags b1 b2 = b1 <> b2

consBag :: a -> Bag a -> Bag a
consBag el b = unitBag el `unionBags` b