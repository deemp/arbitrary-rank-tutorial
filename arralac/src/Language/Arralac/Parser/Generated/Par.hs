{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PartialTypeSignatures #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Arralac.Parser.Generated.Par
  ( happyError
  , myLexer
  , pProgram
  , pVar
  , pCon
  , pBool
  , pExp1
  , pExp2
  , pExp3
  , pExp4
  , pExp5
  , pExp6
  , pExp7
  , pExp
  , pExp10
  , pExp11
  , pType1
  , pType2
  , pType3
  , pTypeVariable
  , pType4
  , pListTypeVariable
  , pType
  ) where

import Prelude

import qualified Language.Arralac.Parser.Generated.Abs
import Language.Arralac.Parser.Generated.Lex
import qualified Data.Text
import qualified Data.Function as Happy_Prelude
import qualified Data.Bool as Happy_Prelude
import qualified Data.Function as Happy_Prelude
import qualified Data.Maybe as Happy_Prelude
import qualified Data.Int as Happy_Prelude
import qualified Data.String as Happy_Prelude
import qualified Data.Tuple as Happy_Prelude
import qualified Data.List as Happy_Prelude
import qualified Control.Monad as Happy_Prelude
import qualified Text.Show as Happy_Prelude
import qualified GHC.Num as Happy_Prelude
import qualified GHC.Err as Happy_Prelude
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.1.5

data HappyAbsSyn 
        = HappyTerminal (Token)
        | HappyErrorToken Happy_Prelude.Int
        | HappyAbsSyn25 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Integer))
        | HappyAbsSyn26 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, String))
        | HappyAbsSyn27 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.NameLowerCase))
        | HappyAbsSyn28 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.NameUpperCase))
        | HappyAbsSyn29 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Program))
        | HappyAbsSyn30 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Var))
        | HappyAbsSyn31 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Con))
        | HappyAbsSyn32 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Bool))
        | HappyAbsSyn33 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp))
        | HappyAbsSyn43 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Type))
        | HappyAbsSyn46 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.TypeVariable))
        | HappyAbsSyn48 ((Language.Arralac.Parser.Generated.Abs.BNFC'Position, [Language.Arralac.Parser.Generated.Abs.TypeVariable]))

{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["'('","')'","'->'","'.'","'::'","'='","'False'","'True'","'\\\\'","'forall'","'in'","'let'","L_integ","L_quoted","L_NameLowerCase","L_NameUpperCase","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x28\x01\x00\x00\xf6\xff\xff\xff\xfd\xff\xff\xff\x0a\x01\x00\x00\x34\x00\x00\x00\x7b\x01\x00\x00\x5a\x00\x00\x00\x60\x00\x00\x00\x24\x00\x00\x00\x69\x00\x00\x00\x28\x01\x00\x00\x28\x01\x00\x00\x32\x01\x00\x00\x24\x00\x00\x00\x6b\x00\x00\x00\x7e\x00\x00\x00\x47\x01\x00\x00\x7e\x00\x00\x00\x7d\x00\x00\x00\x90\x00\x00\x00\x47\x01\x00\x00\x8c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x01\x00\x00\x57\x01\x00\x00\xa8\x00\x00\x00\x58\x01\x00\x00\xa0\x00\x00\x00\x5d\x01\x00\x00\xb4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x00\x00\x00\xac\x00\x00\x00\xac\x00\x00\x00\xac\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\xac\x00\x00\x00\xac\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x01\x00\x00\x28\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x00\x00\x00\x28\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x32\x01\x00\x00\x28\x01\x00\x00\x26\x00\x00\x00\xc6\x00\x00\x00\xbe\x00\x00\x00\xdc\x00\x00\x00\xd6\x00\x00\x00\xd6\x00\x00\x00\xd6\x00\x00\x00\xf2\x00\x00\x00\xe8\x00\x00\x00\xfc\x00\x00\x00\xf4\x00\x00\x00\xf4\x00\x00\x00\xf4\x00\x00\x00\xf4\x00\x00\x00\xf4\x00\x00\x00\xf4\x00\x00\x00\x1c\x01\x00\x00\x6e\x01\x00\x00\x22\x01\x00\x00\x18\x01\x00\x00\x2c\x01\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x1b\x01\x00\x00\x39\x01\x00\x00\x6e\x01\x00\x00\x00\x00\x00\x00\x24\x01\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x6e\x01\x00\x00\x3a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x01\x00\x00\x28\x01\x00\x00\x37\x01\x00\x00\x28\x01\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x75\x01\x00\x00\x41\x00\x00\x00\x48\x01\x00\x00\x28\x01\x00\x00\x4e\x01\x00\x00\x50\x01\x00\x00\x53\x01\x00\x00\x28\x01\x00\x00\x53\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x54\x00\x00\x00\x1e\x01\x00\x00\x77\x01\x00\x00\x59\x01\x00\x00\x86\x01\x00\x00\x04\x00\x00\x00\x5c\x01\x00\x00\x56\x01\x00\x00\x42\x01\x00\x00\x5a\x01\x00\x00\x66\x00\x00\x00\x78\x00\x00\x00\x08\x01\x00\x00\x43\x01\x00\x00\x21\x00\x00\x00\x0a\x00\x00\x00\x28\x00\x00\x00\x01\x00\x00\x00\x5e\x01\x00\x00\x12\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x01\x00\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x01\x00\x00\xae\x00\x00\x00\x8b\x01\x00\x00\x8d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x91\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\xff\xff\xc8\xff\xff\xff\xc9\xff\xff\xff\xc6\xff\xff\xff\xc5\xff\xff\xff\xbc\xff\xff\xff\xc4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xff\xff\xff\xe8\xff\xff\xff\xe7\xff\xff\xff\xc2\xff\xff\xff\xc0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\xff\xff\xc5\xff\xff\xff\x00\x00\x00\x00\xc4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\xff\xff\xe1\xff\xff\xff\xcc\xff\xff\xff\xcb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\xff\xff\xdf\xff\xff\xff\xe4\xff\xff\xff\xdd\xff\xff\xff\xde\xff\xff\xff\xcf\xff\xff\xff\xce\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\xff\xff\xe3\xff\xff\xff\xe9\xff\xff\xff\xcc\xff\xff\xff\xd6\xff\xff\xff\xd5\xff\xff\xff\xd4\xff\xff\xff\xcb\xff\xff\xff\xd2\xff\xff\xff\xd1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc3\xff\xff\xff\x00\x00\x00\x00\xc7\xff\xff\xff\xca\xff\xff\xff\xcd\xff\xff\xff\xca\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\xff\xff\xdc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\xff\xff\x00\x00\x00\x00\xdb\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x02\x00\x00\x00\x09\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x16\x00\x00\x00\x15\x00\x00\x00\x18\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x16\x00\x00\x00\x13\x00\x00\x00\x18\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x18\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x02\x00\x00\x00\x17\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x15\x00\x00\x00\x04\x00\x00\x00\x17\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x10\x00\x00\x00\x15\x00\x00\x00\x10\x00\x00\x00\x17\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x12\x00\x00\x00\x16\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x10\x00\x00\x00\x16\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x0c\x00\x00\x00\x06\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x12\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0a\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x0a\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0d\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x11\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0b\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0e\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x04\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x12\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x12\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x06\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x12\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x05\x00\x00\x00\x10\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x07\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x03\x00\x00\x00\x06\x00\x00\x00\x04\x00\x00\x00\x06\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x05\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x0b\x00\x00\x00\x11\x00\x00\x00\x11\x00\x00\x00\x05\x00\x00\x00\x0c\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x02\x00\x00\x00\x07\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x06\x00\x00\x00\x0a\x00\x00\x00\x0d\x00\x00\x00\x0b\x00\x00\x00\x12\x00\x00\x00\x12\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x06\x00\x00\x00\x02\x00\x00\x00\x12\x00\x00\x00\xff\xff\xff\xff\x03\x00\x00\x00\x16\x00\x00\x00\x0c\x00\x00\x00\x06\x00\x00\x00\x02\x00\x00\x00\xff\xff\xff\xff\x0b\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x0c\x00\x00\x00\x06\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x12\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x02\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x08\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\xff\xff\xff\xff\x05\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x22\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x21\x00\x00\x00\x35\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x17\x00\x00\x00\x53\x00\x00\x00\x22\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x22\x00\x00\x00\x1c\x00\x00\x00\x26\x00\x00\x00\x1d\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x22\x00\x00\x00\x1c\x00\x00\x00\x2b\x00\x00\x00\x6f\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x22\x00\x00\x00\x1c\x00\x00\x00\x18\x00\x00\x00\x75\x00\x00\x00\x33\x00\x00\x00\x23\x00\x00\x00\x5d\x00\x00\x00\x24\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x67\x00\x00\x00\x63\x00\x00\x00\x23\x00\x00\x00\x63\x00\x00\x00\x64\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x2c\x00\x00\x00\x21\x00\x00\x00\x23\x00\x00\x00\x21\x00\x00\x00\x63\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\xff\xff\xff\xff\x2a\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x65\x00\x00\x00\x21\x00\x00\x00\x2a\x00\x00\x00\x5b\x00\x00\x00\x5b\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x68\x00\x00\x00\x6c\x00\x00\x00\x2a\x00\x00\x00\x75\x00\x00\x00\x5b\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x73\x00\x00\x00\xff\xff\xff\xff\x2a\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x58\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x59\x00\x00\x00\x53\x00\x00\x00\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x51\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4b\x00\x00\x00\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x22\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x46\x00\x00\x00\x20\x00\x00\x00\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x21\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x61\x00\x00\x00\x17\x00\x00\x00\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x21\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x60\x00\x00\x00\x63\x00\x00\x00\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\xff\xff\xff\xff\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x5e\x00\x00\x00\xff\xff\xff\xff\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x21\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x72\x00\x00\x00\xff\xff\xff\xff\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x21\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x70\x00\x00\x00\x5b\x00\x00\x00\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\xff\xff\xff\xff\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x77\x00\x00\x00\x5d\x00\x00\x00\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\xff\xff\xff\xff\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\x79\x00\x00\x00\xff\xff\xff\xff\x47\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x21\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x39\x00\x00\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x35\x00\x00\x00\x3a\x00\x00\x00\x2e\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x39\x00\x00\x00\x6b\x00\x00\x00\x3c\x00\x00\x00\x2d\x00\x00\x00\x5b\x00\x00\x00\x5b\x00\x00\x00\x57\x00\x00\x00\x5f\x00\x00\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x6f\x00\x00\x00\x21\x00\x00\x00\x68\x00\x00\x00\x49\x00\x00\x00\x17\x00\x00\x00\x3f\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\xff\xff\xff\xff\x3d\x00\x00\x00\x3e\x00\x00\x00\x4a\x00\x00\x00\x6d\x00\x00\x00\x3c\x00\x00\x00\x4b\x00\x00\x00\x17\x00\x00\x00\x3f\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x6a\x00\x00\x00\x72\x00\x00\x00\x63\x00\x00\x00\x5b\x00\x00\x00\x17\x00\x00\x00\x3f\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x2d\x00\x00\x00\x2d\x00\x00\x00\x5b\x00\x00\x00\x2e\x00\x00\x00\x2e\x00\x00\x00\x1f\x00\x00\x00\x2f\x00\x00\x00\x2f\x00\x00\x00\x63\x00\x00\x00\xbe\xff\xff\xff\x4e\x00\x00\x00\x30\x00\x00\x00\xbe\xff\xff\xff\x77\x00\x00\x00\x20\x00\x00\x00\x47\x00\x00\x00\x31\x00\x00\x00\x79\x00\x00\x00\xbe\xff\xff\xff\x21\x00\x00\x00\x22\x00\x00\x00\x5b\x00\x00\x00\xbd\xff\xff\xff\xbb\xff\xff\xff\xbe\xff\xff\xff\xbd\xff\xff\xff\xbb\xff\xff\xff\x1f\x00\x00\x00\x55\x00\x00\x00\x4f\x00\x00\x00\xd7\xff\xff\xff\xbd\xff\xff\xff\xbb\xff\xff\xff\xd7\xff\xff\xff\x51\x00\x00\x00\x4d\x00\x00\x00\x20\x00\x00\x00\xbd\xff\xff\xff\xbb\xff\xff\xff\xd7\xff\xff\xff\xd3\xff\xff\xff\x21\x00\x00\x00\x22\x00\x00\x00\xd3\xff\xff\xff\x1f\x00\x00\x00\xd7\xff\xff\xff\x00\x00\x00\x00\xd0\xff\xff\xff\x25\x00\x00\x00\xd3\xff\xff\xff\xd0\xff\xff\xff\x1f\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x35\x00\x00\x00\xd3\xff\xff\xff\xd0\xff\xff\xff\x56\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\xd0\xff\xff\xff\x3d\x00\x00\x00\x3e\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x17\x00\x00\x00\x3f\x00\x00\x00\x2e\x00\x00\x00\x22\x00\x00\x00\x2d\x00\x00\x00\x54\x00\x00\x00\x2d\x00\x00\x00\x5b\x00\x00\x00\x2d\x00\x00\x00\x5d\x00\x00\x00\x2d\x00\x00\x00\x5b\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (21, 68) [
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66),
        (67 , happyReduce_67),
        (68 , happyReduce_68)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x01\x00\x00\x00\x06\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x04\x00\x00\x00\x0b\x00\x00\x00\x08\x00\x00\x00\x0c\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x06\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x03\x00\x00\x00\x14\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x03\x00\x00\x00\x15\x00\x00\x00\x01\x00\x00\x00\x16\x00\x00\x00\x04\x00\x00\x00\x17\x00\x00\x00\x00\x00\x00\x00\x17\x00\x00\x00\x02\x00\x00\x00\x18\x00\x00\x00\x01\x00\x00\x00\x18\x00\x00\x00\x01\x00\x00\x00\x18\x00\x00\x00\x01\x00\x00\x00\x18\x00\x00\x00\x01\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 19 :: Happy_Prelude.Int
happy_n_nonterms = 25 :: Happy_Prelude.Int

happy_n_starts = 21 :: Happy_Prelude.Int

happyReduce_21 = happySpecReduce_1  0# happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
         =  HappyAbsSyn25
                 ((uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1), (read (Data.Text.unpack (tokenText happy_var_1))) :: Integer)
        )
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  1# happyReduction_22
happyReduction_22 (HappyTerminal happy_var_1)
         =  HappyAbsSyn26
                 ((uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1), (Data.Text.unpack ((\(PT _ _ (TL s)) -> s) happy_var_1)))
        )
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  2# happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
         =  HappyAbsSyn27
                 ((uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1), Language.Arralac.Parser.Generated.Abs.NameLowerCase (tokenText happy_var_1))
        )
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  3# happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
         =  HappyAbsSyn28
                 ((uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1), Language.Arralac.Parser.Generated.Abs.NameUpperCase (tokenText happy_var_1))
        )
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  4# happyReduction_25
happyReduction_25 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn29
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.Program (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  5# happyReduction_26
happyReduction_26 (HappyAbsSyn27  happy_var_1)
         =  HappyAbsSyn30
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.Var (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  6# happyReduction_27
happyReduction_27 (HappyAbsSyn28  happy_var_1)
         =  HappyAbsSyn31
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.Con (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  7# happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
         =  HappyAbsSyn32
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)), Language.Arralac.Parser.Generated.Abs.BoolTrue (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1))))
        )
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  7# happyReduction_29
happyReduction_29 (HappyTerminal happy_var_1)
         =  HappyAbsSyn32
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)), Language.Arralac.Parser.Generated.Abs.BoolFalse (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1))))
        )
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  8# happyReduction_30
happyReduction_30 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.ExpVar (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  9# happyReduction_31
happyReduction_31 (HappyAbsSyn25  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.ExpInt (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  9# happyReduction_32
happyReduction_32 (HappyAbsSyn26  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.ExpString (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  9# happyReduction_33
happyReduction_33 (HappyAbsSyn32  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.ExpBool (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  9# happyReduction_34
happyReduction_34 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.ExpCon (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happyReduce 4# 10# happyReduction_35
happyReduction_35 ((HappyAbsSyn33  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn30  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4), Language.Arralac.Parser.Generated.Abs.ExpAbs (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4)) (snd happy_var_2) (snd happy_var_4))
        ) `HappyStk` happyRest

happyReduce_36 = happyReduce 8# 11# happyReduction_36
happyReduction_36 ((HappyAbsSyn33  happy_var_8) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn43  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn30  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_8), Language.Arralac.Parser.Generated.Abs.ExpAbsAnno (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_8)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_8))
        ) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_2  12# happyReduction_37
happyReduction_37 (HappyAbsSyn33  happy_var_2)
        (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2), Language.Arralac.Parser.Generated.Abs.ExpApp (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2)) (snd happy_var_1) (snd happy_var_2))
        )
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 6# 13# happyReduction_38
happyReduction_38 ((HappyAbsSyn33  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn33  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn30  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_6), Language.Arralac.Parser.Generated.Abs.ExpLet (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_6)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
        ) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_3  14# happyReduction_39
happyReduction_39 (HappyAbsSyn43  happy_var_3)
        _
        (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Language.Arralac.Parser.Generated.Abs.ExpAnno (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_3))
        )
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  15# happyReduction_40
happyReduction_40 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  15# happyReduction_41
happyReduction_41 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  15# happyReduction_42
happyReduction_42 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  15# happyReduction_43
happyReduction_43 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  15# happyReduction_44
happyReduction_44 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  15# happyReduction_45
happyReduction_45 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  15# happyReduction_46
happyReduction_46 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  15# happyReduction_47
happyReduction_47 (HappyTerminal happy_var_3)
        (HappyAbsSyn33  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_3)), (snd happy_var_2))
        )
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  16# happyReduction_48
happyReduction_48 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  16# happyReduction_49
happyReduction_49 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  16# happyReduction_50
happyReduction_50 (HappyTerminal happy_var_3)
        (HappyAbsSyn33  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_3)), (snd happy_var_2))
        )
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  17# happyReduction_51
happyReduction_51 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  17# happyReduction_52
happyReduction_52 (HappyAbsSyn33  happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  17# happyReduction_53
happyReduction_53 (HappyTerminal happy_var_3)
        (HappyAbsSyn33  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn33
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_3)), (snd happy_var_2))
        )
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  18# happyReduction_54
happyReduction_54 (HappyAbsSyn28  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.TypeConcrete (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  19# happyReduction_55
happyReduction_55 (HappyAbsSyn27  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.TypeVariable (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  20# happyReduction_56
happyReduction_56 (HappyAbsSyn43  happy_var_3)
        _
        (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Language.Arralac.Parser.Generated.Abs.TypeFunc (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_3))
        )
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  20# happyReduction_57
happyReduction_57 (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  20# happyReduction_58
happyReduction_58 (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  20# happyReduction_59
happyReduction_59 (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  20# happyReduction_60
happyReduction_60 (HappyTerminal happy_var_3)
        (HappyAbsSyn43  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_3)), (snd happy_var_2))
        )
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  21# happyReduction_61
happyReduction_61 (HappyAbsSyn27  happy_var_1)
         =  HappyAbsSyn46
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Language.Arralac.Parser.Generated.Abs.TypeVariableName (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
        )
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happyReduce 4# 22# happyReduction_62
happyReduction_62 ((HappyAbsSyn43  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn48  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4), Language.Arralac.Parser.Generated.Abs.TypeForall (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4)) (snd happy_var_2) (snd happy_var_4))
        ) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_0  23# happyReduction_63
happyReduction_63  =  HappyAbsSyn48
                 ((Language.Arralac.Parser.Generated.Abs.BNFC'NoPosition, [])
        )

happyReduce_64 = happySpecReduce_2  23# happyReduction_64
happyReduction_64 (HappyAbsSyn48  happy_var_2)
        (HappyAbsSyn46  happy_var_1)
         =  HappyAbsSyn48
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2), (:) (snd happy_var_1) (snd happy_var_2))
        )
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  24# happyReduction_65
happyReduction_65 (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  24# happyReduction_66
happyReduction_66 (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  24# happyReduction_67
happyReduction_67 (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  24# happyReduction_68
happyReduction_68 (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 ((Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
        )
happyReduction_68 _  = notHappyAtAll 

happyTerminalToTok term = case term of {
        PT _ _ (TS _ 1) -> 2#;
        PT _ _ (TS _ 2) -> 3#;
        PT _ _ (TS _ 3) -> 4#;
        PT _ _ (TS _ 4) -> 5#;
        PT _ _ (TS _ 5) -> 6#;
        PT _ _ (TS _ 6) -> 7#;
        PT _ _ (TS _ 7) -> 8#;
        PT _ _ (TS _ 8) -> 9#;
        PT _ _ (TS _ 9) -> 10#;
        PT _ _ (TS _ 10) -> 11#;
        PT _ _ (TS _ 11) -> 12#;
        PT _ _ (TS _ 12) -> 13#;
        PT _ _ (TI _) -> 14#;
        PT _ _ (TL _) -> 15#;
        PT _ _ (T_NameLowerCase _) -> 16#;
        PT _ _ (T_NameUpperCase _) -> 17#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend  _kmore []       = kend notHappyAtAll []
happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 18# notHappyAtAll action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 18# tk explist resume tks = happyReport' tks explist resume
happyReport _ tk explist resume tks = happyReport' (tk:tks) explist (\tks -> resume (Happy_Prelude.tail tks))


happyThen :: () => (Err a) -> (a -> (Err b)) -> (Err b)
happyThen = ((>>=))
happyReturn :: () => a -> (Err a)
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyFmap1 f m tks = happyThen (m tks) (\a -> happyReturn (f a))
happyReturn1 :: () => a -> b -> (Err a)
happyReturn1 = \a tks -> (return) a
happyReport' :: () => [(Token)] -> [Happy_Prelude.String] -> ([(Token)] -> (Err a)) -> (Err a)
happyReport' = (\tokens expected resume -> happyError tokens)

happyAbort :: () => [(Token)] -> (Err a)
happyAbort = Happy_Prelude.error "Called abort handler in non-resumptive parser"

pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

pVar_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pCon_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

pBool_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

pExp1_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp2_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp3_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp4_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp5_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp6_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp7_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp10_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pExp11_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pType1_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

pType2_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

pType3_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

pTypeVariable_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> case x of {HappyAbsSyn46 z -> happyReturn z; _other -> notHappyAtAll })

pType4_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

pListTypeVariable_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> case x of {HappyAbsSyn48 z -> happyReturn z; _other -> notHappyAtAll })

pType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: Data.Text.Text -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Program
pProgram = fmap snd . pProgram_internal

pVar :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Var
pVar = fmap snd . pVar_internal

pCon :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Con
pCon = fmap snd . pCon_internal

pBool :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Bool
pBool = fmap snd . pBool_internal

pExp1 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp1 = fmap snd . pExp1_internal

pExp2 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp2 = fmap snd . pExp2_internal

pExp3 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp3 = fmap snd . pExp3_internal

pExp4 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp4 = fmap snd . pExp4_internal

pExp5 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp5 = fmap snd . pExp5_internal

pExp6 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp6 = fmap snd . pExp6_internal

pExp7 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp7 = fmap snd . pExp7_internal

pExp :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp = fmap snd . pExp_internal

pExp10 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp10 = fmap snd . pExp10_internal

pExp11 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Exp
pExp11 = fmap snd . pExp11_internal

pType1 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Type
pType1 = fmap snd . pType1_internal

pType2 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Type
pType2 = fmap snd . pType2_internal

pType3 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Type
pType3 = fmap snd . pType3_internal

pTypeVariable :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.TypeVariable
pTypeVariable = fmap snd . pTypeVariable_internal

pType4 :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Type
pType4 = fmap snd . pType4_internal

pListTypeVariable :: [Token] -> Err [Language.Arralac.Parser.Generated.Abs.TypeVariable]
pListTypeVariable = fmap snd . pListTypeVariable_internal

pType :: [Token] -> Err Language.Arralac.Parser.Generated.Abs.Type
pType = fmap snd . pType_internal
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Happy_Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Happy_Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Happy_Prelude.Bool)
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define INVALID_TOK -1#
#define ERROR_TOK 0#
#define CATCH_TOK 1#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) Happy_Prelude.$
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO Happy_Prelude.$ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    Happy_Prelude.return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++
              ",\ttoken: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# rule) Happy_Prelude.++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Happy_Prelude.Just (Happy_GHC_Exts.I# act) -> act
  Happy_Prelude.Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(i, 0#), GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  -- i >= 0:   Guard against INVALID_TOK (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Happy_Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | Happy_Prelude.otherwise
  = Happy_Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state
  deriving Happy_Prelude.Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | Happy_Prelude.otherwise = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr PLUS(offs,1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     -- See "Error Fixup" below
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

{- Note [Error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~
When there is no applicable action for the current lookahead token `tk`,
happy enters error recovery mode. Depending on whether the grammar file
declares the two action form `%error { abort } { report }` for
    Resumptive Error Handling,
it works in one (not resumptive) or two phases (resumptive):

 1. Fixup mode:
    Try to see if there is an action for the error token ERROR_TOK. If there
    is, do *not* emit an error and pretend instead that an `error` token was
    inserted.
    When there is no ERROR_TOK action, report an error.

    In non-resumptive error handling, calling the single error handler
    (e.g. `happyError`) will throw an exception and abort the parser.
    However, in resumptive error handling we enter *error resumption mode*.

 2. Error resumption mode:
    After reporting the error (with `report`), happy will attempt to find
    a good state stack to resume parsing in.
    For each candidate stack, it discards input until one of the candidates
    resumes (i.e. shifts the current input).
    If no candidate resumes before the end of input, resumption failed and
    calls the `abort` function, to much the same effect as in non-resumptive
    error handling.

    Candidate stacks are declared by the grammar author using the special
    `catch` terminal and called "catch frames".
    This mechanism is described in detail in Note [happyResume].

The `catch` resumption mechanism (2) is what usually is associated with
`error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
(1) above, we call the corresponding token `catch`.
Furthermore, in constrast to `bison`, our implementation of `catch`
non-deterministically considers multiple catch frames on the stack for
resumption (See Note [Multiple catch frames]).

Note [happyResume]
~~~~~~~~~~~~~~~~~~
`happyResume` implements the resumption mechanism from Note [Error recovery].
It is best understood by example. Consider

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 Happy_Prelude.++ " + " Happy_Prelude.++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" Happy_Prelude.++ $2 Happy_Prelude.++ ")" }

The idea of the use of `catch` here is that upon encountering a parse error
during expression parsing, we can gracefully degrade using the `catch` rule,
still producing a partial syntax tree and keep on parsing to find further
syntax errors.

Let's trace the parser state for input 11+1, which will error out after shifting 1.
After shifting, we have the following item stack (growing downwards and omitting
transitive closure items):

  State 0: %start_parseExp -> . Exp
  State 5: Exp -> '1' .

(Stack as a list of state numbers: [5,0].)
As Note [Error recovery] describes, we will first try Fixup mode.
That fails because no production can shift the `error` token.
Next we try Error resumption mode. This works as follows:

  1. Pop off the item stack until we find an item that can shift the `catch`
     token. (Implemented in `pop_items`.)
       * State 5 cannot shift catch. Pop.
       * State 0 can shift catch, which would transition into
          State 4: Exp -> catch .
     So record the *stack* `[4,0]` after doing the shift transition.
     We call this a *catch frame*, where the top is a *catch state*,
     corresponding to an item in which we just shifted a `catch` token.
     There can be multiple such catch stacks, see Note [Multiple catch frames].

  2. Discard tokens from the input until the lookahead can be shifted in one
     of the catch stacks. (Implemented in `discard_input_until_exp` and
     `some_catch_state_shifts`.)
       * We cannot shift the current lookahead '1' in state 4, so we discard
       * We *can* shift the next lookahead '+' in state 4, but only after
         reducing, which pops State 4 and goes to State 3:
           State 3: %start_parseExp -> Exp .
                    Exp -> Exp . '+' Exp
         Here we can shift '+'.
     As you can see, to implement this machinery we need to simulate
     the operation of the LALR automaton, especially reduction
     (`happySimulateReduce`).

Note [Multiple catch frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For fewer spurious error messages, it can be beneficial to trace multiple catch
items. Consider

Exp : '1'
    | catch
    | Exp '+' Exp %shift
    | '(' Exp ')'

Let's trace the parser state for input (;+1, which will error out after shifting (.
After shifting, we have the following item stack (growing downwards):

  State 0: %start_parseExp -> . Exp
  State 6: Exp -> '(' . Exp ')'

Upon error, we want to find items in the stack which can shift a catch token.
Note that both State 0 and State 6 can shift a catch token, transitioning into
  State 4: Exp -> catch .
Hence we record the catch frames `[4,6,0]` and `[4,0]` for possible resumption.

Which catch frame do we pick for resumption?
Note that resuming catch frame `[4,0]` will parse as "catch+1", whereas
resuming the innermost frame `[4,6,0]` corresponds to parsing "(catch+1".
The latter would keep discarding input until the closing ')' is found.
So we will discard + and 1, leading to a spurious syntax error at the end of
input, aborting the parse and never producing a partial syntax tree. Bad!

It is far preferable to resume with catch frame `[4,0]`, where we can resume
successfully on input +, so that is what we do.

In general, we pick the catch frame for resumption that discards the least
amount of input for a successful shift, preferring the topmost such catch frame.
-}

-- happyFail :: Happy_Int -> Token -> Happy_Int -> _
-- This function triggers Note [Error recovery].
-- If the current token is ERROR_TOK, phase (1) has failed and we might try
-- phase (2).
happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

-- Enter Error Fixup (see Note [Error recovery]):
-- generate an error token, save the old token and carry on.
-- When a `happyShift` accepts the error token, we will pop off the error token
-- to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- See Note [Error recovery], phase (2).
-- Enter resumption mode after reporting the error by calling `happyResume`.
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  happyReport i tk expected resume

-- happyResume :: Happy_Int -> Token -> Happy_Int -> _
-- See Note [happyResume]
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms Happy_Prelude.- 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Happy_Prelude.Int]
    happy_list_to_list (HappyCons st sts)
      | LT(st, n_starts)
      = [(Happy_GHC_Exts.I# st)]
      | Happy_Prelude.otherwise
      = (Happy_GHC_Exts.I# st) : happy_list_to_list sts

    -- See (1) of Note [happyResume]
    pop_items catch_frames st sts stk
      | LT(st, n_starts)
      = DEBUG_TRACE("reached start state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", ")
        if Happy_Prelude.null catch_frames_new
          then DEBUG_TRACE("no resumption.\n")
               happyAbort
          else DEBUG_TRACE("now discard input, trying to anchor in states " Happy_Prelude.++ Happy_Prelude.show (Happy_Prelude.map (happy_list_to_list . Happy_Prelude.fst) (Happy_Prelude.reverse catch_frames_new)) Happy_Prelude.++ ".\n")
               discard_input_until_exp i tk (Happy_Prelude.reverse catch_frames_new)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_frames_new st1 sts1 stk1
      where
        !catch_frames_new
          | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
          , DEBUG_TRACE("can shift catch token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", into state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
            Happy_Prelude.null (Happy_Prelude.filter (\(HappyCons _ (HappyCons h _),_) -> EQ(st,h)) catch_frames)
          = (HappyCons new_state (HappyCons st sts), MK_ERROR_TOKEN(i) `HappyStk` stk):catch_frames -- MK_ERROR_TOKEN(i) is just some dummy that should not be accessed by user code
          | Happy_Prelude.otherwise
          = DEBUG_TRACE("already shifted or can't shift catch in " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ "\n")
            catch_frames

    -- See (2) of Note [happyResume]
    discard_input_until_exp i tk catch_frames
      | Happy_Prelude.Just (HappyCons st (HappyCons catch_st sts), catch_frame) <- some_catch_state_shifts i catch_frames
      = DEBUG_TRACE("found expected token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ " after shifting from " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# catch_st) Happy_Prelude.++ ": " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyDoAction i tk st (HappyCons catch_st sts) catch_frame
      | EQ(i,eof_i) -- is i EOF?
      = DEBUG_TRACE("reached EOF, cannot resume. abort parse :(\n")
        happyAbort
      | Happy_Prelude.otherwise
      = DEBUG_TRACE("discard token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_frames) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_frames)         -- not eof

    some_catch_state_shifts _ [] = DEBUG_TRACE("no catch state could shift.\n") Happy_Prelude.Nothing
    some_catch_state_shifts i catch_frames@(((HappyCons st sts),_):_) = try_head i st sts catch_frames
      where
        try_head i st sts catch_frames = -- PRECONDITION: head catch_frames = (HappyCons st sts)
          DEBUG_TRACE("trying token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ " in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ": ")
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> DEBUG_TRACE("fail.\n")   some_catch_state_shifts i (Happy_Prelude.tail catch_frames)
            HappyAccept   -> DEBUG_TRACE("accept.\n") Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyShift _  -> DEBUG_TRACE("shift.\n")  Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_frames

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# r) Happy_Prelude.++ ", ")
  let (# nt, len #) = happyIndexRuleArr r in
  DEBUG_TRACE("nt " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# nt) Happy_Prelude.++ ", len: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# len) Happy_Prelude.++ ", new_st ")
  let !(sts1@(HappyCons st1 _)) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(Happy_Prelude.show (Happy_GHC_Exts.I# new_st) Happy_Prelude.++ ".\n")
  (HappyCons new_st sts1)

happyTokenToString :: Happy_Prelude.Int -> Happy_Prelude.String
happyTokenToString i = happyTokenStrings Happy_Prelude.!! (i Happy_Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Happy_Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  Happy_Prelude.map happyTokenToString (search_shifts st sts [])
  where
    search_shifts st sts shifts = Happy_Prelude.foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", input " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ ", " Happy_Prelude.++ Happy_Prelude.show (happyDecodeAction act) Happy_Prelude.++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Prelude.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = ((-1), (Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st)))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [(Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i))]
      | Happy_Prelude.otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Happy_Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Happy_GHC_Exts.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
