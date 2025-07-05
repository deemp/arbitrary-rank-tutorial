{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

module Language.STLC.Typing.Jones2007.Main where

-- import Data.Maybe

import Prettyprinter

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (newIORef, readIORef)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Text
import Data.Text.IO qualified as T
import Data.Validation (Validation (..))
import Language.STLC.Common (parseWith)
import Language.STLC.Syntax.Abs qualified as Abs
import Language.STLC.Syntax.Par (pProgram)
import Language.STLC.Typing.Jones2007.BasicTypes qualified as BT
import Language.STLC.Typing.Jones2007.TcMonad
import Language.STLC.Typing.Jones2007.TcTerm
import Prettyprinter.Render.Text (putDoc)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Prelude hiding (exp)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [] -> getContents >>= tcs . pack
--     [f] -> tcf f
--     _ -> do
--       hPutStrLn stderr "Usage: foo [ FILE ]"
--       exitWith (ExitFailure 1)

-------------------------------------
--  tcs type-checks an expression passed directly as a string
--  ghci> tcs "\\x. x"

-- tcs :: Text -> IO () -- Strings
-- tcs s = tc_help (parseString s)

-- s1, s2 :: String
-- s1 = "\\x. \\y. x" -- A tiny example
-- s2 =
--   "let add = (\\x. \\y. x) :: forall a. a -> a -> a in \
--   \ let id  = (\\x. x) :: forall a. a -> a in \
--   \ add id id"

-- >>> t1
-- Nothing

-------------------------------------
--  tcf type-checks an expression in a file
--  ghci> tcs "foo.test"

-- tcf :: String -> IO () -- Files
-- tcf f = tc_help (parseFile f)

-------------------------------------
--  The initial type environment.
--  You can extend this as you like

-- tyvarA :: BT.TyVar
-- tyvarA = BT.BoundTv Nothing "a"

-- initTypeEnv :: [(BT.Name, BT.Sigma)]
-- initTypeEnv =
--   [ (BT.Name Nothing "+", BT.intType BT.--> BT.intType BT.--> BT.intType)
--   , (BT.Name Nothing "if", BT.ForAll Nothing [tyvarA] (BT.boolType BT.--> BT.TyVar Nothing tyvarA BT.--> BT.TyVar Nothing tyvarA))
--   , (BT.Name Nothing "True", BT.boolType)
--   , (BT.Name Nothing "False", BT.boolType)
--   ]

-------------------------------------
--  From here down is helper stuff

-- tc_help :: IO (Maybe (BT.Term)) -> IO ()
-- tc_help get_term =
--   do
--     mb_e <- get_term
--     case mb_e of
--       Nothing -> return ()
--       Just e -> do
--         res <- runTc initTypeEnv (typecheck e)
--         case res of
--           Left err -> putStrLn (BT.docToString err)
--           Right ty -> putStrLn (BT.docToString (sep [BT.pprParendTerm e, nest 2 (BT.dcolon <+> BT.ppr ty)]))

-- TODO convert to a typeclass with an associated type?
-- Or, directly replace term with

-- TODO validate
-- forall introduces non-zero type variables
convertProgram :: (BT.IConvertRename) => Abs.Program -> IO (BT.SynTerm BT.CompRn)
convertProgram (Abs.Program _ exp) = BT.convertAbsToBT exp

-- convertExp :: Abs.Exp -> Validation [Doc Text] (BT.Term Abs.BNFC'Position)
-- convertExp = \case
--   Abs.ExpVar ann (Abs.Var ann1 (Abs.NameLowerCase x)) -> pure $ BT.Var ann (BT.Name ann1 x)
--   Abs.ExpInt ann x -> pure $ BT.Lit ann x
--   Abs.ExpAbs ann (Abs.Var ann1 (Abs.NameLowerCase x)) exp -> BT.Lam ann (BT.Name ann1 x) <$> convertExp exp
--   Abs.ExpAbsAnno ann (Abs.Var ann1 (Abs.NameLowerCase x)) t exp -> BT.ALam ann (BT.Name ann1 x) <$> convertType t <*> convertExp exp
--   Abs.ExpApp ann exp1 exp2 -> BT.App ann <$> convertExp exp1 <*> convertExp exp2
--   Abs.ExpLet ann (Abs.Var ann1 (Abs.NameLowerCase x)) exp1 exp2 -> BT.Let ann (BT.Name ann1 x) <$> convertExp exp1 <*> convertExp exp2
--   Abs.ExpAnno ann exp t -> BT.Ann ann <$> convertExp exp <*> convertType t

-- convertType :: Abs.Type -> Validation [Doc Text] (BT.Type Abs.BNFC'Position)
-- convertType = \case
--   Abs.TypeConcrete ann (Abs.NameUpperCase x) ->
--     case x of
--       "Int" -> pure $ BT.TyCon ann BT.IntT
--       "Bool" -> pure $ BT.TyCon ann BT.BoolT
--       _ -> Failure ["Unknown type '" <> pretty x <> "' at: " <> pretty ann]
--   Abs.TypeVariable ann (Abs.NameLowerCase x) -> pure $ BT.TyVar ann (BT.BoundTv ann x)
--   Abs.TypeFunc ann t1 t2 -> BT.Fun ann <$> convertType t1 <*> convertType t2
--   Abs.TypeForall ann ts t1 ->
--     case ts of
--       [] -> Failure ["No variables bound at: " <> pretty ann]
--       ts' -> BT.ForAll ann ((\(Abs.TypeVariableName ann1 (Abs.NameLowerCase x)) -> BT.BoundTv ann1 x) <$> ts') <$> convertType t1

data ParseErrorWithCallStack where
  ParseErrorWithCallStack :: (HasCallStack) => String -> ParseErrorWithCallStack

instance Show ParseErrorWithCallStack where
  show (ParseErrorWithCallStack err) = prettyCallStack callStack <> "\n\n" <> err

instance Exception ParseErrorWithCallStack

parseInputText :: Text -> RnM (BT.SynTerm BT.CompRn)
parseInputText input = do
  let
    parsed = parseWith pProgram input
  case parsed of
    -- TODO throw better and handle where necessary
    Left err -> throw (ParseErrorWithCallStack err)
    Right prog -> convertProgram prog

t3 :: IO (Doc ann)
t3 = do
  uniqueSupply <- newIORef 0
  tcError <- newIORef Nothing
  let
    ?uniqueSupply = uniqueSupply
    ?tcLevel = BT.TcLevel 0
    ?varEnv = Map.empty
    ?scope = Map.empty
    ?debug = True
    ?tcError = tcError
   in
    do
      exp <- parseFile "test/data/Program1.stlc"
      debug "t3" [pretty exp]
      pretty <$> typecheck exp

parseInput :: (BT.IConvertRename) => Either String Text -> IO (BT.SynTerm BT.CompRn)
parseInput input = do
  input' <- either T.readFile pure input
  parseInputText input'

parseFile :: (BT.IConvertRename) => String -> IO (BT.SynTerm BT.CompRn)
parseFile filename = parseInput (Left filename)

parseString :: (BT.IConvertRename) => Text -> IO (BT.SynTerm BT.CompRn)
parseString input = parseInput (Right input)

-- TODO produce a fully annotated term
