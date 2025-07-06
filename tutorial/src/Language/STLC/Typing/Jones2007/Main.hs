{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.STLC.Typing.Jones2007.Main where

-- import Data.Maybe

import Prettyprinter

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.STLC.Typing.Jones2007.TcTerm
import Language.STLC.Typing.Pass.Renamed
import Prelude hiding (exp)

t3 :: RnM (Doc ann)
t3 = do
  let filePath = "test/data/Program1.stlc"
  content <- T.readFile filePath
  programZn <- runTypechecker' (T.pack filePath) content
  pure $ pretty programZn

-- >>> t3
-- "Exception!"
