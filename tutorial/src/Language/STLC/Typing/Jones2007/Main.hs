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

import Control.Monad.Foil (emptyScope)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.STLC.Interpreter.Main (convertASTToCore, whnf)
import Language.STLC.Typing.Jones2007.BasicTypes (Pretty' (..), PrettyVerbosity (PrettyVerbosity'Compact))
import Language.STLC.Typing.Jones2007.TcTerm
import Prettyprinter.Render.Text
import Prelude hiding (exp)

t3 :: IO ()
t3 = do
  let filePath = "test/data/Program1.stlc"
  content <- T.readFile filePath
  let ?debug = True
      ?prettyVerbosity = PrettyVerbosity'Normal
  programZn <- runTypechecker' (T.pack filePath) content
  putDoc $ line <> pretty' programZn <> line
  putDoc $ line <> pretty' (whnf emptyScope (convertASTToCore emptyScope programZn)) <> line

-- >>> t3
-- "Exception!"
