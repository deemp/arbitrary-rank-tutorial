{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.STLC.Typing.Jones2007.Main where

-- import Data.Maybe

import Language.STLC.Typing.Jones2007.BasicTypes
import Prettyprinter

import Control.Monad (forM_)
import Data.Text
import Data.Text.IO qualified as T
import Data.Validation (Validation (..))
import Language.STLC.Common (parseWith)
import Language.STLC.Syntax.Abs as Abs
import Language.STLC.Syntax.Par (pProgram)
import Language.STLC.Typing.Jones2007.BasicTypes as BT
import Language.STLC.Typing.Jones2007.TcMonad
import Language.STLC.Typing.Jones2007.TcTerm
import Prettyprinter.Render.Text (putDoc)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Prelude hiding (exp)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= tcs . pack
    [f] -> tcf f
    _ -> do
      hPutStrLn stderr "Usage: foo [ FILE ]"
      exitWith (ExitFailure 1)

-------------------------------------
--  tcs type-checks an expression passed directly as a string
--  ghci> tcs "\\x. x"

tcs :: Text -> IO () -- Strings
tcs s = tc_help (parseString s)

s1, s2 :: String
s1 = "\\x. \\y. x" -- A tiny example
s2 =
  "let add = (\\x. \\y. x) :: forall a. a -> a -> a in \
  \ let id  = (\\x. x) :: forall a. a -> a in \
  \ add id id"

-- >>> t1
-- Nothing

-------------------------------------
--  tcf type-checks an expression in a file
--  ghci> tcs "foo.test"

tcf :: String -> IO () -- Files
tcf f = tc_help (parseFile f)

-------------------------------------
--  The initial type environment.
--  You can extend this as you like

tyvarA :: TyVar
tyvarA = BoundTv "a"

initTypeEnv :: [(Name, Sigma)]
initTypeEnv =
  [ ("+", intType --> intType --> intType)
  , ("if", ForAll [tyvarA] (boolType --> TyVar tyvarA --> TyVar tyvarA))
  , ("True", boolType)
  , ("False", boolType)
  ]

-------------------------------------
--  From here down is helper stuff

tc_help :: IO (Maybe Term) -> IO ()
tc_help get_term =
  do
    mb_e <- get_term
    case mb_e of
      Nothing -> return ()
      Just e -> do
        res <- runTc initTypeEnv (typecheck e)
        case res of
          Left err -> putStrLn (docToString err)
          Right ty -> putStrLn (docToString (sep [pprParendTerm e, nest 2 (dcolon <+> ppr ty)]))

-- TODO validate
-- forall introduces non-zero type variables
convertProgram :: Program -> Validation [Doc Text] Term
convertProgram = \case
  Program _ exp -> convertExp exp

convertExp :: Exp -> Validation [Doc Text] Term
convertExp = \case
  ExpVar _ (Abs.Var _ (NameLowerCase x)) -> pure $ BT.Var x
  ExpInt _ x -> pure $ BT.Lit x
  ExpAbs _ (Abs.Var _ (NameLowerCase x)) exp -> Lam x <$> convertExp exp
  ExpAbsAnno _ (Abs.Var _ (NameLowerCase x)) t exp -> ALam x <$> convertType t <*> convertExp exp
  ExpApp _ exp1 exp2 -> BT.App <$> convertExp exp1 <*> convertExp exp2
  ExpLet _ (Abs.Var _ (NameLowerCase x)) exp1 exp2 -> BT.Let x <$> convertExp exp1 <*> convertExp exp2
  ExpAnno _ exp t -> BT.Ann <$> convertExp exp <*> convertType t

convertType :: Abs.Type -> Validation [Doc Text] BT.Type
convertType = \case
  TypeConcrete pos (NameUpperCase x) ->
    case x of
      "Int" -> pure $ BT.TyCon BT.IntT
      "Bool" -> pure $ BT.TyCon BT.BoolT
      _ -> Failure ["Unknown type '" <> pretty x <> "' at: " <> pretty pos]
  TypeVariable _ (NameLowerCase x) -> pure $ BT.TyVar (BT.BoundTv x)
  TypeFunc _ t1 t2 -> Fun <$> convertType t1 <*> convertType t2
  TypeForall pos ts t1 ->
    case ts of
      [] -> Failure ["No variables bound at: " <> pretty pos]
      ts' -> BT.ForAll ((\(NameLowerCase x) -> BoundTv x) <$> ts') <$> convertType t1

parseInputPure :: Text -> Validation [Doc Text] Term
parseInputPure input = do
  let
    parsed = parseWith pProgram input
  case parsed of
    Left err -> Failure [pretty err]
    Right prog -> convertProgram prog

t3 :: IO (Either (Doc Text) Sigma)
t3 =
  case parseInputPure (pack s2) of
    Failure err -> pure $ Left $ vsep err
    Success t -> runTc initTypeEnv (typecheck t)

-- >>> t3
-- Right (forall a. a -> a)

parseInput :: Either String Text -> IO (Maybe Term)
parseInput input =
  do
    input' <- either T.readFile pure input
    let prog = parseInputPure input'
    case prog of
      Failure errors -> do
        forM_ errors putDoc
        pure Nothing
      Success program'' -> pure (Just program'')

parseFile :: String -> IO (Maybe Term)
parseFile filename = parseInput (Left filename)

parseString :: Text -> IO (Maybe Term)
parseString input = parseInput (Right input)
