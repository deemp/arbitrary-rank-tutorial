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

tyvarA :: BT.TyVar (Maybe ann)
tyvarA = BT.BoundTv Nothing "a"

initTypeEnv :: [(BT.Name (Maybe ann), BT.Sigma (Maybe ann))]
initTypeEnv =
  [ (BT.Name Nothing "+", BT.intType BT.--> BT.intType BT.--> BT.intType)
  , (BT.Name Nothing "if", BT.ForAll Nothing [tyvarA] (BT.boolType BT.--> BT.TyVar Nothing tyvarA BT.--> BT.TyVar Nothing tyvarA))
  , (BT.Name Nothing "True", BT.boolType)
  , (BT.Name Nothing "False", BT.boolType)
  ]

-------------------------------------
--  From here down is helper stuff

tc_help :: IO (Maybe (BT.Term (Maybe ann))) -> IO ()
tc_help get_term =
  do
    mb_e <- get_term
    case mb_e of
      Nothing -> return ()
      Just e -> do
        res <- runTc initTypeEnv (typecheck e)
        case res of
          Left err -> putStrLn (BT.docToString err)
          Right ty -> putStrLn (BT.docToString (sep [BT.pprParendTerm e, nest 2 (BT.dcolon <+> BT.ppr ty)]))

-- TODO convert to a typeclass with an associated type?
-- Or, directly replace term with

-- TODO validate
-- forall introduces non-zero type variables
convertProgram :: Abs.Program -> Validation [Doc Text] (BT.Term Abs.BNFC'Position)
convertProgram = \case
  Abs.Program _ exp -> convertExp exp

convertExp :: Abs.Exp -> Validation [Doc Text] (BT.Term Abs.BNFC'Position)
convertExp = \case
  Abs.ExpVar ann (Abs.Var ann1 (Abs.NameLowerCase x)) -> pure $ BT.Var ann (BT.Name ann1 x)
  Abs.ExpInt ann x -> pure $ BT.Lit ann x
  Abs.ExpAbs ann (Abs.Var ann1 (Abs.NameLowerCase x)) exp -> BT.Lam ann (BT.Name ann1 x) <$> convertExp exp
  Abs.ExpAbsAnno ann (Abs.Var ann1 (Abs.NameLowerCase x)) t exp -> BT.ALam ann (BT.Name ann1 x) <$> convertType t <*> convertExp exp
  Abs.ExpApp ann exp1 exp2 -> BT.App ann <$> convertExp exp1 <*> convertExp exp2
  Abs.ExpLet ann (Abs.Var ann1 (Abs.NameLowerCase x)) exp1 exp2 -> BT.Let ann (BT.Name ann1 x) <$> convertExp exp1 <*> convertExp exp2
  Abs.ExpAnno ann exp t -> BT.Ann ann <$> convertExp exp <*> convertType t

convertType :: Abs.Type -> Validation [Doc Text] (BT.Type Abs.BNFC'Position)
convertType = \case
  Abs.TypeConcrete ann (Abs.NameUpperCase x) ->
    case x of
      "Int" -> pure $ BT.TyCon ann BT.IntT
      "Bool" -> pure $ BT.TyCon ann BT.BoolT
      _ -> Failure ["Unknown type '" <> pretty x <> "' at: " <> pretty ann]
  Abs.TypeVariable ann (Abs.NameLowerCase x) -> pure $ BT.TyVar ann (BT.BoundTv ann x)
  Abs.TypeFunc ann t1 t2 -> BT.Fun ann <$> convertType t1 <*> convertType t2
  Abs.TypeForall ann ts t1 ->
    case ts of
      [] -> Failure ["No variables bound at: " <> pretty ann]
      ts' -> BT.ForAll ann ((\(Abs.TypeVariableName ann1 (Abs.NameLowerCase x)) -> BT.BoundTv ann1 x) <$> ts') <$> convertType t1

parseInputPure :: Text -> Validation [Doc Text] (BT.Term Abs.BNFC'Position)
parseInputPure input = do
  let
    parsed = parseWith pProgram input
  case parsed of
    Left err -> Failure [pretty err]
    Right prog -> convertProgram prog

class MkMaybeAnn (a :: Type -> Type) where
  mkMaybeAnn :: a b -> IO (a (Maybe b))

instance MkMaybeAnn BT.Term where
  mkMaybeAnn = \case
    BT.Var ann name -> BT.Var (Just ann) <$> (mkMaybeAnn name)
    BT.Lit ann name -> pure $ BT.Lit (Just ann) name
    BT.App ann term1 term2 -> BT.App (Just ann) <$> (mkMaybeAnn term1) <*> (mkMaybeAnn term2)
    BT.Lam ann name term -> BT.Lam (Just ann) <$> (mkMaybeAnn name) <*> (mkMaybeAnn term)
    BT.ALam ann name ty t -> BT.ALam (Just ann) <$> (mkMaybeAnn name) <*> (mkMaybeAnn ty) <*> (mkMaybeAnn t)
    BT.Let ann name term1 term2 -> BT.Let (Just ann) <$> (mkMaybeAnn name) <*> (mkMaybeAnn term1) <*> (mkMaybeAnn term2)
    BT.Ann ann term ty -> BT.Ann (Just ann) <$> (mkMaybeAnn term) <*> (mkMaybeAnn ty)

instance MkMaybeAnn BT.Name where
  mkMaybeAnn (BT.Name ann t) = pure $ BT.Name (Just ann) t

instance MkMaybeAnn BT.Type where
  mkMaybeAnn = \case
    BT.ForAll ann tys ty -> BT.ForAll (Just ann) <$> (forM tys mkMaybeAnn) <*> (mkMaybeAnn ty)
    BT.Fun ann ty1 ty2 -> BT.Fun (Just ann) <$> (mkMaybeAnn ty1) <*> (mkMaybeAnn ty2)
    BT.TyCon ann ty -> pure $ BT.TyCon (Just ann) ty
    BT.TyVar ann ty -> BT.TyVar (Just ann) <$> (mkMaybeAnn ty)
    BT.MetaTv ann ty -> BT.MetaTv (Just ann) <$> (mkMaybeAnn ty)

instance MkMaybeAnn BT.MetaTv where
  mkMaybeAnn (BT.Meta ann u r) = do
    r1 <- liftIO $ readIORef r
    r2 <-
      case r1 of
        Nothing -> pure Nothing
        Just r3 -> pure <$> mkMaybeAnn r3
    r3 <- newIORef r2
    pure $ BT.Meta (Just ann) u r3

instance MkMaybeAnn BT.TyVar where
  mkMaybeAnn = \case
    BT.BoundTv ann t -> pure $ BT.BoundTv (Just ann) t
    BT.SkolemTv ann t u -> pure $ BT.SkolemTv (Just ann) t u

t3 :: IO (Either (Doc Text) (BT.Sigma (Maybe Abs.BNFC'Position)))
t3 =
  case parseInputPure (pack s2) of
    Failure err -> pure $ Left $ vsep err
    Success t -> do
      t1 <- mkMaybeAnn t
      runTc initTypeEnv (typecheck t1)

-- >>> t3
-- Right (forall a. a -> a)

parseInput :: Either String Text -> IO (Maybe (BT.Term Abs.BNFC'Position))
parseInput input =
  do
    input' <- either T.readFile pure input
    let prog = parseInputPure input'
    case prog of
      Failure errors -> do
        forM_ errors putDoc
        pure Nothing
      Success program'' -> pure (Just program'')

parseFile :: String -> IO (Maybe (BT.Term Abs.BNFC'Position))
parseFile filename = parseInput (Left filename)

parseString :: Text -> IO (Maybe (BT.Term Abs.BNFC'Position))
parseString input = parseInput (Right input)

-- TODO produce a fully annotated term
