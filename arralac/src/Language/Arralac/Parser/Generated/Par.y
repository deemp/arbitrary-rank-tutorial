-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.6).

-- Parser definition for use with Happy
{
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

}

%name pProgram_internal Program
%name pVar_internal Var
%name pCon_internal Con
%name pBool_internal Bool
%name pExp1_internal Exp1
%name pExp2_internal Exp2
%name pExp3_internal Exp3
%name pExp4_internal Exp4
%name pExp5_internal Exp5
%name pExp6_internal Exp6
%name pExp7_internal Exp7
%name pExp_internal Exp
%name pExp10_internal Exp10
%name pExp11_internal Exp11
%name pType1_internal Type1
%name pType2_internal Type2
%name pType3_internal Type3
%name pTypeVariable_internal TypeVariable
%name pType4_internal Type4
%name pListTypeVariable_internal ListTypeVariable
%name pType_internal Type
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('             { PT _ _ (TS _ 1)            }
  ')'             { PT _ _ (TS _ 2)            }
  '->'            { PT _ _ (TS _ 3)            }
  '.'             { PT _ _ (TS _ 4)            }
  '::'            { PT _ _ (TS _ 5)            }
  '='             { PT _ _ (TS _ 6)            }
  'False'         { PT _ _ (TS _ 7)            }
  'True'          { PT _ _ (TS _ 8)            }
  '\\'            { PT _ _ (TS _ 9)            }
  'forall'        { PT _ _ (TS _ 10)           }
  'in'            { PT _ _ (TS _ 11)           }
  'let'           { PT _ _ (TS _ 12)           }
  L_integ         { PT _ _ (TI _)              }
  L_quoted        { PT _ _ (TL _)              }
  L_NameLowerCase { PT _ _ (T_NameLowerCase _) }
  L_NameUpperCase { PT _ _ (T_NameUpperCase _) }

%%

Integer :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1), (read (Data.Text.unpack (tokenText $1))) :: Integer) }

String  :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, String) }
String   : L_quoted { (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1), (Data.Text.unpack ((\(PT _ _ (TL s)) -> s) $1))) }

NameLowerCase :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.NameLowerCase) }
NameLowerCase  : L_NameLowerCase { (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1), Language.Arralac.Parser.Generated.Abs.NameLowerCase (tokenText $1)) }

NameUpperCase :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.NameUpperCase) }
NameUpperCase  : L_NameUpperCase { (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1), Language.Arralac.Parser.Generated.Abs.NameUpperCase (tokenText $1)) }

Program :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Program) }
Program
  : Exp { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.Program (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Var :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Var) }
Var
  : NameLowerCase { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.Var (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Con :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Con) }
Con
  : NameUpperCase { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.Con (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Bool :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Bool) }
Bool
  : 'True' { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)), Language.Arralac.Parser.Generated.Abs.BoolTrue (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)))) }
  | 'False' { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)), Language.Arralac.Parser.Generated.Abs.BoolFalse (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)))) }

Exp1 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp1
  : Var { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.ExpVar (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Exp2 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp2
  : Integer { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.ExpInt (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | String { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.ExpString (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | Bool { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.ExpBool (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | Con { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.ExpCon (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Exp3 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp3
  : '\\' Var '.' Exp { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (fst $4), Language.Arralac.Parser.Generated.Abs.ExpAbs (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (fst $4)) (snd $2) (snd $4)) }

Exp4 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp4
  : '\\' '(' Var '::' Type ')' '.' Exp { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (fst $8), Language.Arralac.Parser.Generated.Abs.ExpAbsAnno (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (fst $8)) (snd $3) (snd $5) (snd $8)) }

Exp5 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp5
  : Exp11 Exp10 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $2), Language.Arralac.Parser.Generated.Abs.ExpApp (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $2)) (snd $1) (snd $2)) }

Exp6 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp6
  : 'let' Var '=' Exp 'in' Exp { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (fst $6), Language.Arralac.Parser.Generated.Abs.ExpLet (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (fst $6)) (snd $2) (snd $4) (snd $6)) }

Exp7 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp7
  : Exp '::' Type { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $3), Language.Arralac.Parser.Generated.Abs.ExpAnno (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $3)) (snd $1) (snd $3)) }

Exp :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp
  : Exp1 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Exp2 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Exp3 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Exp4 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Exp5 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Exp6 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Exp7 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | '(' Exp ')' { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $3)), (snd $2)) }

Exp10 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp10
  : Exp1 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Exp2 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | '(' Exp ')' { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $3)), (snd $2)) }

Exp11 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Exp) }
Exp11
  : Exp1 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Exp5 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | '(' Exp ')' { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $3)), (snd $2)) }

Type1 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Type) }
Type1
  : NameUpperCase { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.TypeConcrete (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Type2 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Type) }
Type2
  : NameLowerCase { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.TypeVariable (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Type3 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Type) }
Type3
  : Type3 '->' Type3 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $3), Language.Arralac.Parser.Generated.Abs.TypeFunc (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $3)) (snd $1) (snd $3)) }
  | Type1 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Type2 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Type4 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | '(' Type3 ')' { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $3)), (snd $2)) }

TypeVariable :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.TypeVariable) }
TypeVariable
  : NameLowerCase { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), Language.Arralac.Parser.Generated.Abs.TypeVariableName (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Type4 :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Type) }
Type4
  : 'forall' ListTypeVariable '.' Type3 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (fst $4), Language.Arralac.Parser.Generated.Abs.TypeForall (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (uncurry Language.Arralac.Parser.Generated.Abs.BNFC'Position (tokenSpan $1)) (fst $4)) (snd $2) (snd $4)) }

ListTypeVariable :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, [Language.Arralac.Parser.Generated.Abs.TypeVariable]) }
ListTypeVariable
  : {- empty -} { (Language.Arralac.Parser.Generated.Abs.BNFC'NoPosition, []) }
  | TypeVariable ListTypeVariable { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $2), (:) (snd $1) (snd $2)) }

Type :: { (Language.Arralac.Parser.Generated.Abs.BNFC'Position, Language.Arralac.Parser.Generated.Abs.Type) }
Type
  : Type1 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Type2 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Type3 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }
  | Type4 { (Language.Arralac.Parser.Generated.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }

{

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
}

