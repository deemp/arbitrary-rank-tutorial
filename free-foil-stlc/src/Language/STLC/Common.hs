{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.STLC.Common where

import Data.String (IsString (..))
import Language.STLC.Syntax.Abs
import Language.STLC.Syntax.Lex (Token)
import Language.STLC.Syntax.Par

-- TODO import only in doctests

import Data.Text (Text, pack, unpack)
import Language.STLC.Syntax.Print

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> newtype Pretty = Pretty String; instance Show Pretty where show (Pretty s) = s

instance IsString Exp where fromString = unsafeParseWith pExp . pack
instance IsString Type where fromString = unsafeParseWith pType . pack
instance IsString Program where fromString = unsafeParseWith pProgram . pack
-- instance IsString Statement where fromString = unsafeParseWith pStatement . pack

parseWith :: ([Token] -> Either String a) -> Text -> Either String a
parseWith parser input = parser tokens
 where
  tokens = myLexer input

-- | Parse from a 'String'.
-- May throw an 'error` if input has syntactical or lexical errors.
unsafeParseWith :: ([Token] -> Either String a) -> Text -> a
unsafeParseWith parser input =
  case parseWith parser input of
    Left parseError -> error (parseError <> "\non input\n" <> unpack input <> "\n")
    Right res -> res

-- >>> "Int -> Int" :: Type
-- TypeFunc (Just (1,1)) (TypeName (Just (1,1)) (NameUpperCase "Int")) (TypeName (Just (1,8)) (NameUpperCase "Int"))

-- >>> "#typecheck x : Int |- \\ x -> (\\ z -> z) x <= Int -> Int" :: Statement
-- StatementCommandTypeCheck (Just (1,1)) (ExpUnderCtx (Just (1,12)) (Ctx (Just (1,12)) [CtxVar (Just (1,12)) (Var (Just (1,12)) (NameLowerCase "x")) (TypeName (Just (1,16)) (NameUpperCase "Int"))]) (ExpAbs (Just (1,23)) (Var (Just (1,25)) (NameLowerCase "x")) (ExpApp (Just (1,30)) (ExpAbs (Just (1,31)) (Var (Just (1,33)) (NameLowerCase "z")) (ExpVar (Just (1,38)) (Var (Just (1,38)) (NameLowerCase "z")))) (ExpVar (Just (1,41)) (Var (Just (1,41)) (NameLowerCase "x")))))) (TypeFunc (Just (1,46)) (TypeName (Just (1,46)) (NameUpperCase "Int")) (TypeName (Just (1,53)) (NameUpperCase "Int")))

-- >>> printModule name = Pretty . (\x -> "==========\n" <> name <> "\n==========\n" <> x) . printTree . fromString @Program <$> (readFile ("free-foil-stlc/test/data/" <> name))
-- >>> printModule "Program.stlc"
-- ==========
-- Program.stlc
-- ==========
-- ex1 = if (((y) (1)) (2)) then
-- {
--   (x) (true)
-- }
-- else
-- {
--   (x) (3)
-- }
-- where
-- {
--   x = \ a -> 5;
--   y = \ a -> \ b -> true
-- };
-- #typecheck |- ex1 <= Int;
-- #typesynth |- (ex1) + (ex1) => Int;
-- #typesynth |- ex1 => ?
