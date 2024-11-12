{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.STLC.Syntax.Par
  ( happyError
  , myLexer
  , pExp1
  , pExp2
  , pExp3
  , pExp4
  , pExp
  , pType1
  , pType2
  , pType
  , pCtxVar
  , pListCtxVar
  , pCtx
  , pExpUnderCtx
  , pCommand
  , pListCommand
  ) where

import Prelude

import qualified Language.STLC.Syntax.Abs
import Language.STLC.Syntax.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn17 (Language.STLC.Syntax.Abs.Var)
	| HappyAbsSyn18 (Language.STLC.Syntax.Abs.Exp)
	| HappyAbsSyn23 (Language.STLC.Syntax.Abs.Type)
	| HappyAbsSyn26 (Language.STLC.Syntax.Abs.CtxVar)
	| HappyAbsSyn27 ([Language.STLC.Syntax.Abs.CtxVar])
	| HappyAbsSyn28 (Language.STLC.Syntax.Abs.Ctx)
	| HappyAbsSyn29 (Language.STLC.Syntax.Abs.ExpUnderCtx)
	| HappyAbsSyn30 (Language.STLC.Syntax.Abs.Command)
	| HappyAbsSyn31 ([Language.STLC.Syntax.Abs.Command])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,154) ([0,0,9222,0,0,4099,0,32768,2049,0,16384,1024,0,24576,576,0,4096,64,0,2048,32,0,1024,16,0,0,32,0,0,16,0,0,8,0,0,4,0,24,0,0,12,0,0,32768,0,0,0,0,0,256,0,0,0,0,0,2048,0,0,1024,0,0,0,0,0,4,0,16384,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,2050,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,6144,128,0,0,0,0,0,0,0,0,0,0,384,9,0,0,0,0,0,2,0,0,0,0,0,0,0,16396,0,0,0,0,0,0,0,0,16,0,0,1,0,8192,128,0,16384,0,0,6144,144,0,0,64,0,512,8,0,0,0,0,32768,0,0,48,0,0,0,0,0,16400,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,576,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pExp1","%start_pExp2","%start_pExp3","%start_pExp4","%start_pExp","%start_pType1","%start_pType2","%start_pType","%start_pCtxVar","%start_pListCtxVar","%start_pCtx","%start_pExpUnderCtx","%start_pCommand","%start_pListCommand","Var","Exp1","Exp2","Exp3","Exp4","Exp","Type1","Type2","Type","CtxVar","ListCtxVar","Ctx","ExpUnderCtx","Command","ListCommand","'#typecheck'","'#typesynth'","'('","'()'","')'","','","'->'","'.'","':'","';'","'<='","'\\\\'","'unit'","'|-'","L_Var","%eof"]
        bit_start = st Prelude.* 47
        bit_end = (st Prelude.+ 1) Prelude.* 47
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..46]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (34) = happyShift action_42
action_0 (35) = happyShift action_43
action_0 (43) = happyShift action_44
action_0 (46) = happyShift action_15
action_0 (17) = happyGoto action_36
action_0 (18) = happyGoto action_48
action_0 (19) = happyGoto action_38
action_0 (20) = happyGoto action_39
action_0 (21) = happyGoto action_40
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (34) = happyShift action_42
action_1 (35) = happyShift action_43
action_1 (46) = happyShift action_15
action_1 (17) = happyGoto action_36
action_1 (19) = happyGoto action_47
action_1 (20) = happyGoto action_39
action_1 (21) = happyGoto action_40
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (34) = happyShift action_42
action_2 (35) = happyShift action_43
action_2 (46) = happyShift action_15
action_2 (17) = happyGoto action_36
action_2 (20) = happyGoto action_46
action_2 (21) = happyGoto action_40
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (34) = happyShift action_42
action_3 (46) = happyShift action_15
action_3 (17) = happyGoto action_36
action_3 (21) = happyGoto action_45
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (34) = happyShift action_42
action_4 (35) = happyShift action_43
action_4 (43) = happyShift action_44
action_4 (46) = happyShift action_15
action_4 (17) = happyGoto action_36
action_4 (18) = happyGoto action_37
action_4 (19) = happyGoto action_38
action_4 (20) = happyGoto action_39
action_4 (21) = happyGoto action_40
action_4 (22) = happyGoto action_41
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (34) = happyShift action_32
action_5 (44) = happyShift action_33
action_5 (23) = happyGoto action_35
action_5 (24) = happyGoto action_30
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (34) = happyShift action_32
action_6 (44) = happyShift action_33
action_6 (24) = happyGoto action_34
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (34) = happyShift action_32
action_7 (44) = happyShift action_33
action_7 (23) = happyGoto action_29
action_7 (24) = happyGoto action_30
action_7 (25) = happyGoto action_31
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (46) = happyShift action_15
action_8 (17) = happyGoto action_21
action_8 (26) = happyGoto action_28
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (46) = happyShift action_15
action_9 (17) = happyGoto action_21
action_9 (26) = happyGoto action_22
action_9 (27) = happyGoto action_27
action_9 _ = happyReduce_30

action_10 (46) = happyShift action_15
action_10 (17) = happyGoto action_21
action_10 (26) = happyGoto action_22
action_10 (27) = happyGoto action_23
action_10 (28) = happyGoto action_26
action_10 _ = happyReduce_30

action_11 (46) = happyShift action_15
action_11 (17) = happyGoto action_21
action_11 (26) = happyGoto action_22
action_11 (27) = happyGoto action_23
action_11 (28) = happyGoto action_24
action_11 (29) = happyGoto action_25
action_11 _ = happyReduce_30

action_12 (32) = happyShift action_18
action_12 (33) = happyShift action_19
action_12 (30) = happyGoto action_20
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (32) = happyShift action_18
action_13 (33) = happyShift action_19
action_13 (30) = happyGoto action_16
action_13 (31) = happyGoto action_17
action_13 _ = happyReduce_37

action_14 (46) = happyShift action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_14

action_16 (41) = happyShift action_59
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (47) = happyAccept
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (46) = happyShift action_15
action_18 (17) = happyGoto action_21
action_18 (26) = happyGoto action_22
action_18 (27) = happyGoto action_23
action_18 (28) = happyGoto action_24
action_18 (29) = happyGoto action_58
action_18 _ = happyReduce_30

action_19 (46) = happyShift action_15
action_19 (17) = happyGoto action_21
action_19 (26) = happyGoto action_22
action_19 (27) = happyGoto action_23
action_19 (28) = happyGoto action_24
action_19 (29) = happyGoto action_57
action_19 _ = happyReduce_30

action_20 (47) = happyAccept
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (40) = happyShift action_56
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (37) = happyShift action_55
action_22 _ = happyReduce_31

action_23 _ = happyReduce_33

action_24 (45) = happyShift action_54
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (47) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (47) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (47) = happyAccept
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (47) = happyAccept
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (38) = happyShift action_52
action_29 _ = happyReduce_28

action_30 _ = happyReduce_25

action_31 (47) = happyAccept
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (34) = happyShift action_32
action_32 (44) = happyShift action_33
action_32 (23) = happyGoto action_29
action_32 (24) = happyGoto action_30
action_32 (25) = happyGoto action_53
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_26

action_34 (47) = happyAccept
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (38) = happyShift action_52
action_35 (47) = happyAccept
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_21

action_37 _ = happyReduce_23

action_38 (34) = happyShift action_42
action_38 (35) = happyShift action_43
action_38 (46) = happyShift action_15
action_38 (17) = happyGoto action_36
action_38 (20) = happyGoto action_49
action_38 (21) = happyGoto action_40
action_38 _ = happyReduce_16

action_39 _ = happyReduce_18

action_40 _ = happyReduce_20

action_41 (47) = happyAccept
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (34) = happyShift action_42
action_42 (35) = happyShift action_43
action_42 (43) = happyShift action_44
action_42 (46) = happyShift action_15
action_42 (17) = happyGoto action_36
action_42 (18) = happyGoto action_37
action_42 (19) = happyGoto action_38
action_42 (20) = happyGoto action_39
action_42 (21) = happyGoto action_40
action_42 (22) = happyGoto action_51
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_19

action_44 (46) = happyShift action_15
action_44 (17) = happyGoto action_50
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (47) = happyAccept
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (47) = happyAccept
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (34) = happyShift action_42
action_47 (35) = happyShift action_43
action_47 (46) = happyShift action_15
action_47 (47) = happyAccept
action_47 (17) = happyGoto action_36
action_47 (20) = happyGoto action_49
action_47 (21) = happyGoto action_40
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (47) = happyAccept
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_17

action_50 (39) = happyShift action_68
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (36) = happyShift action_67
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (34) = happyShift action_32
action_52 (44) = happyShift action_33
action_52 (24) = happyGoto action_66
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (36) = happyShift action_65
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (34) = happyShift action_42
action_54 (35) = happyShift action_43
action_54 (43) = happyShift action_44
action_54 (46) = happyShift action_15
action_54 (17) = happyGoto action_36
action_54 (18) = happyGoto action_37
action_54 (19) = happyGoto action_38
action_54 (20) = happyGoto action_39
action_54 (21) = happyGoto action_40
action_54 (22) = happyGoto action_64
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (46) = happyShift action_15
action_55 (17) = happyGoto action_21
action_55 (26) = happyGoto action_22
action_55 (27) = happyGoto action_63
action_55 _ = happyReduce_30

action_56 (34) = happyShift action_32
action_56 (44) = happyShift action_33
action_56 (23) = happyGoto action_29
action_56 (24) = happyGoto action_30
action_56 (25) = happyGoto action_62
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_36

action_58 (42) = happyShift action_61
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (32) = happyShift action_18
action_59 (33) = happyShift action_19
action_59 (30) = happyGoto action_16
action_59 (31) = happyGoto action_60
action_59 _ = happyReduce_37

action_60 _ = happyReduce_38

action_61 (34) = happyShift action_32
action_61 (44) = happyShift action_33
action_61 (23) = happyGoto action_29
action_61 (24) = happyGoto action_30
action_61 (25) = happyGoto action_70
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_29

action_63 _ = happyReduce_32

action_64 _ = happyReduce_34

action_65 _ = happyReduce_27

action_66 _ = happyReduce_24

action_67 _ = happyReduce_22

action_68 (34) = happyShift action_42
action_68 (35) = happyShift action_43
action_68 (43) = happyShift action_44
action_68 (46) = happyShift action_15
action_68 (17) = happyGoto action_36
action_68 (18) = happyGoto action_37
action_68 (19) = happyGoto action_38
action_68 (20) = happyGoto action_39
action_68 (21) = happyGoto action_40
action_68 (22) = happyGoto action_69
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_15

action_70 _ = happyReduce_35

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyTerminal (PT _ (T_Var happy_var_1)))
	 =  HappyAbsSyn17
		 (Language.STLC.Syntax.Abs.Var happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 18 happyReduction_15
happyReduction_15 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Language.STLC.Syntax.Abs.ExpAbs happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  18 happyReduction_16
happyReduction_16 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  19 happyReduction_17
happyReduction_17 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (Language.STLC.Syntax.Abs.ExpApp happy_var_1 happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  19 happyReduction_18
happyReduction_18 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  20 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn18
		 (Language.STLC.Syntax.Abs.ExpUnit
	)

happyReduce_20 = happySpecReduce_1  20 happyReduction_20
happyReduction_20 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  21 happyReduction_21
happyReduction_21 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (Language.STLC.Syntax.Abs.ExpVar happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  21 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  22 happyReduction_23
happyReduction_23 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  23 happyReduction_24
happyReduction_24 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (Language.STLC.Syntax.Abs.TypeFunc happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  23 happyReduction_25
happyReduction_25 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  24 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn23
		 (Language.STLC.Syntax.Abs.TypeUnit
	)

happyReduce_27 = happySpecReduce_3  24 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  25 happyReduction_28
happyReduction_28 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  26 happyReduction_29
happyReduction_29 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn26
		 (Language.STLC.Syntax.Abs.CtxVar happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  27 happyReduction_30
happyReduction_30  =  HappyAbsSyn27
		 ([]
	)

happyReduce_31 = happySpecReduce_1  27 happyReduction_31
happyReduction_31 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn27
		 ((:[]) happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  27 happyReduction_32
happyReduction_32 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn27
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  28 happyReduction_33
happyReduction_33 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 (Language.STLC.Syntax.Abs.Ctx happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  29 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn29
		 (Language.STLC.Syntax.Abs.ExpUnderCtx happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 4 30 happyReduction_35
happyReduction_35 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (Language.STLC.Syntax.Abs.CommandTypeCheck happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_2  30 happyReduction_36
happyReduction_36 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Language.STLC.Syntax.Abs.CommandTypeSynth happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  31 happyReduction_37
happyReduction_37  =  HappyAbsSyn31
		 ([]
	)

happyReduce_38 = happySpecReduce_3  31 happyReduction_38
happyReduction_38 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 47 47 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 32;
	PT _ (TS _ 2) -> cont 33;
	PT _ (TS _ 3) -> cont 34;
	PT _ (TS _ 4) -> cont 35;
	PT _ (TS _ 5) -> cont 36;
	PT _ (TS _ 6) -> cont 37;
	PT _ (TS _ 7) -> cont 38;
	PT _ (TS _ 8) -> cont 39;
	PT _ (TS _ 9) -> cont 40;
	PT _ (TS _ 10) -> cont 41;
	PT _ (TS _ 11) -> cont 42;
	PT _ (TS _ 12) -> cont 43;
	PT _ (TS _ 13) -> cont 44;
	PT _ (TS _ 14) -> cont 45;
	PT _ (T_Var happy_dollar_dollar) -> cont 46;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 47 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pCtxVar tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pListCtxVar tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pCtx tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn28 z -> happyReturn z; _other -> notHappyAtAll })

pExpUnderCtx tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































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
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
