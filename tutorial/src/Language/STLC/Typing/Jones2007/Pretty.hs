{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.STLC.Typing.Jones2007.Pretty where

import GHC.Generics (C, Constructor (..), D, Generic (Rep, from), K1 (..), M1 (..), R, S, Selector (..), U1 (..), (:*:) (..), type (:+:) (..))
import Language.STLC.Typing.Jones2007.BasicTypes (IPrettyVerbosity, Pretty' (..))
import Prettyprinter (Doc, indent, line, vsep, (<+>))

prettyField :: (IPrettyVerbosity, Pretty' a) => a -> Doc ann
prettyField val = line <> indent 2 (pretty' val)

class GPrettyFields f where
  gprettyFields :: (IPrettyVerbosity) => f a -> [Doc ann]

instance (Selector s, Pretty' a) => GPrettyFields (M1 S s (K1 R a)) where
  gprettyFields s@(M1 (K1 val)) =
    let fieldName = pretty' (selName s)
        fieldDoc = fieldName <+> "=" <> prettyField val
     in [fieldDoc]

instance (GPrettyFields f, GPrettyFields g) => GPrettyFields (f :*: g) where
  gprettyFields (f :*: g) = gprettyFields f ++ gprettyFields g

instance GPrettyFields U1 where
  gprettyFields U1 = []

class GPretty f where
  gpretty :: (IPrettyVerbosity) => f a -> Doc ann

instance (GPretty f) => GPretty (M1 D c f) where
  gpretty (M1 x) = gpretty x

instance (GPretty f, GPretty g) => GPretty (f :+: g) where
  gpretty (L1 x) = gpretty x
  gpretty (R1 y) = gpretty y

instance (Constructor c, GPrettyFields f) => GPretty (M1 C c f) where
  gpretty c@(M1 fields) =
    let conName' = pretty' (conName c)
        fieldDocs = gprettyFields fields
     in -- TODO handle non-records?
        if conIsRecord c && not (null fieldDocs)
          then vsep [conName', "{", indent 2 (vsep fieldDocs), "}"]
          else conName'

genericPretty :: (IPrettyVerbosity, Generic a, GPretty (Rep a)) => a -> Doc ann
genericPretty = gpretty . from
