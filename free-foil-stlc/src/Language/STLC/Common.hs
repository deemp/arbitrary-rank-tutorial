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
instance IsString Command where fromString = unsafeParseWith pCommand
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

-- >>> Pretty . printTree . fromString @Program <$> (readFile "free-foil-stlc/test/data/Program.stlc")
-- a : Int;
-- a = (b) c where
-- {
--   b : Int -> Int;
--   b = id;
--   c : Int;
--   c = 3;
-- };
