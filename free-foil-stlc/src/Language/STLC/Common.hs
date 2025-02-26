{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.STLC.Common where

import Data.String (IsString (..))
import Language.STLC.Syntax.Abs
import Language.STLC.Syntax.Lex (Token)
import Language.STLC.Syntax.Par

-- $setup
-- >>> :set -XOverloadedStrings

instance IsString Exp where fromString = unsafeParseWith pExp
instance IsString Type where fromString = unsafeParseWith pType
instance IsString Ctx where fromString = unsafeParseWith pCtx
instance IsString ExpUnderCtx where fromString = unsafeParseWith pExpUnderCtx
instance IsString Command where fromString = unsafeParseWith pCommand

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

-- >>> "#typecheck x : unit |- \\ x . (\\ z . z) x <= unit -> unit" :: Command
-- CommandTypeCheck (ExpUnderCtx (Ctx [CtxVar (Var "x") TypeUnit]) (ExpAbs (Var "x") (ExpApp (ExpAbs (Var "z") (ExpVar (Var "z"))) (ExpVar (Var "x"))))) (TypeFunc TypeUnit TypeUnit)

-- >>> printTree $ ("#typecheck x : unit |- \\x.(\\z.z) x <= unit -> unit" :: Command)
-- "#typecheck x : unit |- \\ x . (\\ z . z) x <= unit -> unit"
