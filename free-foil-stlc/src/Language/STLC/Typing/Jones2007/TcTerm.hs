{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.STLC.Typing.Jones2007.TcTerm where

import Data.IORef
import Data.List ((\\))
import Language.STLC.Typing.Jones2007.BasicTypes
import Language.STLC.Typing.Jones2007.TcMonad
import Prettyprinter

------------------------------------------
--      The top-level wrapper           --
------------------------------------------

typecheck :: SynTerm CompRn -> TcM Sigma
typecheck e = do
  ty <- inferSigma e
  zonkType ty

-----------------------------------
--      The expected type       --
-----------------------------------

data Expected a = Infer (IORef a) | Check a

------------------------------------------
--      tcRho, and its variants         --
------------------------------------------

checkRho :: SynTerm CompRn -> Rho -> TcM ()
-- Invariant: the Rho is always in weak-prenex form
checkRho expr ty = tcRho expr (Check ty)

inferRho :: SynTerm CompRn -> TcM Rho
inferRho expr =
  do
    ref <- newTcRef (error "inferRho: empty result")
    tcRho expr (Infer ref)
    readTcRef ref

-- NOTE
-- Bidirectional type checking
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Gen/HsType.hs#L1030

-- TODO Quick Look
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Gen/App.hs#L79

-- Storing the types
--
-- How does HLS store signatures of expressions?
--
-- Let's save types of each identifier and each parenthesized expression.
--
-- Alternative:
-- When a user requests the type of a parenthesized expression
-- the expression is inferred as a hole.
--
-- A parenthesized expression is a hole.

-- HsExpr GhcTc has Id instead of TcTyVar
-- and it's possible to calculate the type of each sub-expression
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Syn/Type.hs#L104

-- tcExpr: the main expression typechecker
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Gen/Expr.hs#L224
-- The second argument is the `Exp`ected rho-type

-- ExpType
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L401

-- So, tcExpr returns SynTerm CompTc where we have Id instead of TcTyVar
-- TODO separate Type and TcTyVar
-- Maybe make Type a TTG

synTypeToType :: SynType CompRn -> Type
synTypeToType = _

-- TODO return reconstructed SynTerm CompTc
tcRho :: SynTerm CompRn -> Expected Rho -> TcM ()
-- Invariant: if the second argument is (Check rho),
--            then rho is in weak-prenex form
tcRho (SynTerm'Lit _ann _) exp_ty =
  instSigma intType exp_ty
tcRho (SynTerm'Var _ann v) exp_ty = do
  v_sigma <- lookupVar v
  instSigma v_sigma exp_ty
tcRho (SynTerm'App _ann fun arg) exp_ty = do
  fun_ty <- inferRho fun
  (arg_ty, res_ty) <- unifyFun fun_ty
  checkSigma arg arg_ty
  instSigma res_ty exp_ty
tcRho (SynTerm'Lam _ann var body) (Check exp_ty) = do
  (var_ty, body_ty) <- unifyFun exp_ty
  extendVarEnv var var_ty (checkRho body body_ty)
tcRho (SynTerm'Lam _ann var body) (Infer ref) = do
  var_ty <- Type'Var <$> newMetaTyVar' "m"
  body_ty <- extendVarEnv var var_ty (inferRho body)
  writeTcRef ref (var_ty --> body_ty)
tcRho (SynTerm'ALam _ann var var_ty body) (Check exp_ty) = do
  (arg_ty, body_ty) <- unifyFun exp_ty
  let var_ty' = synTypeToType var_ty
  subsCheck arg_ty var_ty'
  extendVarEnv var var_ty' (checkRho body body_ty)
tcRho (SynTerm'ALam _ann var var_ty body) (Infer ref) = do
  let var_ty' = synTypeToType var_ty
  body_ty <- extendVarEnv var var_ty' (inferRho body)
  writeTcRef ref (var_ty' --> body_ty)
tcRho (SynTerm'Let _ann var rhs body) exp_ty = do
  var_ty <- inferSigma rhs
  extendVarEnv var var_ty (tcRho body exp_ty)
tcRho (SynTerm'Ann _ann body ann_ty) exp_ty = do
  let ann_ty' = synTypeToType ann_ty
  checkSigma body ann_ty'
  instSigma ann_ty' exp_ty

------------------------------------------
--      inferSigma and checkSigma
------------------------------------------

inferSigma :: SynTerm CompRn -> TcM Sigma
inferSigma e =
  do
    exp_ty <- inferRho e
    env_tys <- getEnvTypes
    env_tvs <- getMetaTyVars env_tys
    res_tvs <- getMetaTyVars [exp_ty]
    let forall_tvs = res_tvs \\ env_tvs
    quantify forall_tvs exp_ty

checkSigma :: SynTerm CompRn -> Sigma -> TcM ()
checkSigma expr sigma =
  do
    (skol_tvs, rho) <- skolemise sigma
    checkRho expr rho
    env_tys <- getEnvTypes
    esc_tvs <- getFreeTyVars (sigma : env_tys)
    let bad_tvs = filter (`elem` esc_tvs) skol_tvs
    check
      (null bad_tvs)
      ("Type not polymorphic enough")

------------------------------------------
--      Subsumption checking            --
------------------------------------------

subsCheck :: Sigma -> Sigma -> TcM ()
-- (subsCheck args off exp) checks that
--     'off' is at least as polymorphic as 'args -> exp'

subsCheck sigma1 sigma2 -- Rule DEEP-SKOL
  =
  do
    (skol_tvs, rho2) <- skolemise sigma2
    subsCheckRho sigma1 rho2
    esc_tvs <- getFreeTyVars [sigma1, sigma2]
    let bad_tvs = filter (`elem` esc_tvs) skol_tvs
    check
      (null bad_tvs)
      ( vcat
          [ "Subsumption check failed:"
          , nest 2 (pretty sigma1)
          , "is not as polymorphic as"
          , nest 2 (pretty sigma2)
          ]
      )

subsCheckRho :: Sigma -> Rho -> TcM ()
-- Invariant: the second argument is in weak-prenex form

subsCheckRho sigma1@(Type'ForAll _ _) rho2 -- Rule SPEC
  =
  do
    rho1 <- instantiate sigma1
    subsCheckRho rho1 rho2
subsCheckRho rho1 (Type'Fun a2 r2) -- Rule FUN
  =
  do (a1, r1) <- unifyFun rho1; subsCheckFun a1 r1 a2 r2
subsCheckRho (Type'Fun a1 r1) rho2 -- Rule FUN
  =
  do (a2, r2) <- unifyFun rho2; subsCheckFun a1 r1 a2 r2
subsCheckRho tau1 tau2 -- Rule MONO
  =
  unify tau1 tau2 -- Revert to ordinary unification

subsCheckFun :: Sigma -> Rho -> Sigma -> Rho -> TcM ()
subsCheckFun a1 r1 a2 r2 =
  do subsCheck a2 a1; subsCheckRho r1 r2

instSigma :: Sigma -> Expected Rho -> TcM ()
-- Invariant: if the second argument is (Check rho),
--            then rho is in weak-prenex form
instSigma t1 (Check t2) = subsCheckRho t1 t2
instSigma t1 (Infer r) = do
  t1' <- instantiate t1
  writeTcRef r t1'
