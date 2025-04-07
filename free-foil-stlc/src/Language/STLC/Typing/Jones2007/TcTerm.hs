module Language.STLC.Typing.Jones2007.TcTerm where

import Data.IORef
import Data.List ((\\))
import Language.STLC.Typing.Jones2007.BasicTypes
import Language.STLC.Typing.Jones2007.TcMonad
import Prettyprinter

------------------------------------------
--      The top-level wrapper           --
------------------------------------------

typecheck :: Term (Maybe ann) -> Tc ann (Sigma (Maybe ann))
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

checkRho :: Term (Maybe ann) -> Rho (Maybe ann) -> Tc ann ()
-- Invariant: the Rho is always in weak-prenex form
checkRho expr ty = tcRho expr (Check ty)

inferRho :: Term (Maybe ann) -> Tc ann (Rho (Maybe ann))
inferRho expr =
  do
    ref <- newTcRef (error "inferRho: empty result")
    tcRho expr (Infer ref)
    readTcRef ref

tcRho :: Term (Maybe ann) -> Expected (Rho (Maybe ann)) -> Tc ann ()
-- Invariant: if the second argument is (Check rho),
--            then rho is in weak-prenex form
tcRho (Lit _ann _) exp_ty =
  instSigma intType exp_ty
tcRho (Var _ann v) exp_ty =
  do
    v_sigma <- lookupVar v
    instSigma v_sigma exp_ty
tcRho (App _ann fun arg) exp_ty =
  do
    fun_ty <- inferRho fun
    (arg_ty, res_ty) <- unifyFun fun_ty
    checkSigma arg arg_ty
    instSigma res_ty exp_ty
tcRho (Lam _ann var body) (Check exp_ty) =
  do
    (var_ty, body_ty) <- unifyFun exp_ty
    extendVarEnv var var_ty (checkRho body body_ty)
tcRho (Lam _ann var body) (Infer ref) =
  do
    var_ty <- newTyVarTy
    body_ty <- extendVarEnv var var_ty (inferRho body)
    writeTcRef ref (var_ty --> body_ty)
tcRho (ALam _ann var var_ty body) (Check exp_ty) =
  do
    (arg_ty, body_ty) <- unifyFun exp_ty
    subsCheck arg_ty var_ty
    extendVarEnv var var_ty (checkRho body body_ty)
tcRho (ALam _ann var var_ty body) (Infer ref) =
  do
    body_ty <- extendVarEnv var var_ty (inferRho body)
    writeTcRef ref (var_ty --> body_ty)
tcRho (Let _ann var rhs body) exp_ty =
  do
    var_ty <- inferSigma rhs
    extendVarEnv var var_ty (tcRho body exp_ty)
tcRho (Ann _ann body ann_ty) exp_ty =
  do
    checkSigma body ann_ty
    instSigma ann_ty exp_ty

------------------------------------------
--      inferSigma and checkSigma
------------------------------------------

inferSigma :: Term (Maybe ann) -> Tc ann (Sigma (Maybe ann))
inferSigma e =
  do
    exp_ty <- inferRho e
    env_tys <- getEnvTypes
    env_tvs <- getMetaTyVars env_tys
    res_tvs <- getMetaTyVars [exp_ty]
    let forall_tvs = res_tvs \\ env_tvs
    quantify forall_tvs exp_ty

checkSigma :: Term (Maybe ann) -> Sigma (Maybe ann) -> Tc ann ()
checkSigma expr sigma =
  do
    (skol_tvs, rho) <- skolemise sigma
    checkRho expr rho
    env_tys <- getEnvTypes
    esc_tvs <- getFreeTyVars (sigma : env_tys)
    let bad_tvs = filter (`elem` esc_tvs) skol_tvs
    check
      (null bad_tvs)
      (pretty "Type not polymorphic enough")

------------------------------------------
--      Subsumption checking            --
------------------------------------------

subsCheck :: Sigma (Maybe ann) -> Sigma (Maybe ann) -> Tc ann ()
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
          [ pretty "Subsumption check failed:"
          , nest 2 (ppr sigma1)
          , pretty "is not as polymorphic as"
          , nest 2 (ppr sigma2)
          ]
      )

subsCheckRho :: Sigma (Maybe ann) -> Rho (Maybe ann) -> Tc ann ()
-- Invariant: the second argument is in weak-prenex form

subsCheckRho sigma1@(ForAll' _ _) rho2 -- Rule SPEC
  =
  do
    rho1 <- instantiate sigma1
    subsCheckRho rho1 rho2
subsCheckRho rho1 (Fun' a2 r2) -- Rule FUN
  =
  do (a1, r1) <- unifyFun rho1; subsCheckFun a1 r1 a2 r2
subsCheckRho (Fun' a1 r1) rho2 -- Rule FUN
  =
  do (a2, r2) <- unifyFun rho2; subsCheckFun a1 r1 a2 r2
subsCheckRho tau1 tau2 -- Rule MONO
  =
  unify tau1 tau2 -- Revert to ordinary unification

subsCheckFun :: Sigma (Maybe ann) -> Rho (Maybe ann) -> Sigma (Maybe ann) -> Rho (Maybe ann) -> Tc ann ()
subsCheckFun a1 r1 a2 r2 =
  do subsCheck a2 a1; subsCheckRho r1 r2

instSigma :: Sigma (Maybe ann) -> Expected (Rho (Maybe ann)) -> Tc ann ()
-- Invariant: if the second argument is (Check rho),
--            then rho is in weak-prenex form
instSigma t1 (Check t2) = subsCheckRho t1 t2
instSigma t1 (Infer r) = do
  t1' <- instantiate t1
  writeTcRef r t1'
