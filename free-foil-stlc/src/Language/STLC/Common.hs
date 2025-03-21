{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.STLC.Common where

import Data.String (IsString (..))
import Language.STLC.Syntax.Abs
import Language.STLC.Syntax.Lex (Token)
import Language.STLC.Syntax.Par

-- TODO import only in doctests
import Language.STLC.Syntax.Print

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> newtype Pretty = Pretty String; instance Show Pretty where show (Pretty s) = s

instance IsString Exp where fromString = unsafeParseWith pExp
instance IsString Type where fromString = unsafeParseWith pType
instance IsString Ctx where fromString = unsafeParseWith pCtx
instance IsString ExpUnderCtx where fromString = unsafeParseWith pExpUnderCtx
instance IsString Program where fromString = unsafeParseWith pProgram

parseWith :: ([Token] -> Either String a) -> String -> Either String a
parseWith parser input = parser tokens
 where
  tokens = myLexer input

-- | Parse from a 'String'.
-- May throw an 'error` if input has syntactical or lexical errors.
unsafeParseWith :: ([Token] -> Either String a) -> String -> a
unsafeParseWith parser input =
  case parseWith parser input of
    Left parseError -> error (parseError <> "\non input\n" <> input <> "\n")
    Right res -> res

-- >>> "#typecheck x : Int |- \\ x . (\\ z . z) x <= Int -> Int" :: Command
-- CommandTypeCheck (Just (1,1)) (ExpUnderCtx (Just (1,12)) (Ctx (Just (1,12)) [CtxVar (Just (1,12)) (Var "x") (TypeUnit (Just (1,16)))]) (ExpAbs (Just (1,23)) (Var "x") (ExpApp (Just (1,29)) (ExpAbs (Just (1,30)) (Var "z") (ExpVar (Just (1,36)) (Var "z"))) (ExpVar (Just (1,39)) (Var "x"))))) (TypeFunc (Just (1,44)) (TypeUnit (Just (1,44))) (TypeUnit (Just (1,51))))

-- >>> printTree $ ("#typecheck x : Int |- \\x.(\\z.z) x <= Int -> Int" :: Command)
-- "#typecheck x : Int |- \\ x . (\\ z . z) x <= Int -> Int"

-- >>> printModule name = Pretty . (\x -> "==========\n" <> name <> "\n==========\n" <> x) . printTree . fromString @Program <$> (readFile ("free-foil-stlc/test/data/" <> name))
-- >>> printModule "Lib.ms"
-- >>> printModule "Program.ms"
-- ==========
-- Lib.ms
-- ==========
-- externalConst = 314;
-- export externalConst;
-- ==========
-- Program.ms
-- ==========
-- global : Int;
-- global = 3;
-- module Hello where
-- {
-- };
-- module Hello where
-- {
--   import * as Lib from "./Lib.stlc";
--   module Local where
--   {
--     local = global;
--   };
--   application : Int;
--   application = (whereFunction) whereExpression where
--   {
--     whereFunction : Int -> Int;
--     whereFunction = id;
--     whereExpression = global + 5 + # Lib . externalConst + # Local . local;
--   };
--   externalConst = # Lib . externalConst + 10;
--   shadow = 5;
--   export shadow;
--   shadowRef = shadow;
--   shadow = shadow + 8;
--   export shadow;
--   export shadowRef;
--   g = 6 + 7 + application;
--   export application, externalConst, g;
-- };
-- module World where
-- {
--   import * from Hello;
--   shadow = shadow;
--   #typecheck |- shadow <= Int;
--   import
--   {
--     externalConst
--   }
--   from "./Lib.stlc";
--   t = \ p . application + externalConst + global + shadow + g + p;
-- };
-- #typecheck |- # World . t <= Int -> Int;
-- #typesynth |- # World . t => ! Int -> Int;
-- #typesynth s : Int |- # World . shadow + s => ?;
