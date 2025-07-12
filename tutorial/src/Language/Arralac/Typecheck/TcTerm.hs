module Language.Arralac.Typecheck.TcTerm where

import Data.IORef (newIORef, readIORef, writeIORef)
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Syntax.TTG.Type
import Language.Arralac.Typecheck.Bag (unitBag)
import Language.Arralac.Typecheck.Constraints (ImplicStatus (..), Implication (..), TypedThing (..), WantedConstraints (..), emptyWantedConstraints)
import Language.Arralac.Typecheck.Pass
import Language.Arralac.Typecheck.TcMonad
import Language.Arralac.Utils.Pretty
import Prettyprinter (line)

checkRho :: SynTerm CompRn -> Rho -> TcM (SynTerm CompTc)
-- Invariant: the Rho is always in weak-prenex form
checkRho expr ty = do
  debug'
    "checkRho"
    [ ("expr", pretty' expr)
    , ("rho", pretty' ty)
    ]
  tcRho expr (Check ty)

-- TODO should return synterm along with the type?
inferRho :: SynTerm CompRn -> TcM (SynTerm CompTc, Rho)
inferRho expr =
  do
    debug'
      "inferRho"
      [ ("expr", pretty' expr)
      ]
    ref <- newIORef (error "inferRho: empty result")
    expr' <- tcRho expr (Infer ref)
    ref' <- readIORef ref
    pure (expr', ref')

-- TODO Levels
convertSynTy :: SynType CompRn -> TcM (SynType CompTc, TcType)
convertSynTy = \case
  SynType'Var _ var -> do
    -- TODO correct tcLevel?
    var' <- pure TcTyVar{varName = var, varDetails = BoundTv{tcLevel = ?tcLevel}}
    pure $
      ( SynType'Var () var'
      , Type'Var var'
      )
  SynType'ForAll srcLoc tvs body -> do
    -- TODO correct tcLevel?
    let tvs' = (\varName -> TcTyVar{varName, varDetails = BoundTv{tcLevel = ?tcLevel}}) <$> tvs
    (body', body'ty) <- convertSynTy body
    pure $
      ( SynType'ForAll srcLoc tvs' body'
      , Type'ForAll tvs' body'ty
      )
  SynType'Fun srcLoc arg res -> do
    (arg', arg'ty) <- convertSynTy arg
    (res', res'ty) <- convertSynTy res
    pure $
      ( SynType'Fun srcLoc arg' res'
      , Type'Fun arg'ty res'ty
      )
  -- TODO handle more correctly?
  SynType'Concrete srcLoc concrete -> do
    pure $
      ( SynType'Concrete srcLoc concrete
      , Type'Concrete concrete.concreteType
      )

mkTypedThingIfCheck :: SynTerm CompRn -> Expected a -> Maybe TypedThing
mkTypedThingIfCheck thing = \case
  -- TODO should we provide typed thing during inference?
  Infer _ -> Nothing
  Check _ -> Just (HsExprRnThing thing)

-- | Similar to @tcCheckPolyExpr@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Gen/Expr.hs#L100
tcRho :: SynTerm CompRn -> Expected Rho -> TcM (SynTerm CompTc)
-- Invariant: if the second argument is (Check rho),
--            then rho is in weak-prenex form
tcRho t@(SynTerm'Lit annoSrcLoc lit) exp_ty = do
  let ty =
        case lit of
          SynLit'Num{} -> TypeConcrete'Int
          SynLit'Bool{} -> TypeConcrete'Bool
          SynLit'Str{} -> TypeConcrete'String
          SynLit'Con conName -> TypeConcrete'Con conName
  instSigma (mkTypedThingIfCheck t exp_ty) (Type'Concrete ty) exp_ty
  pure $
    SynTerm'Lit
      TcAnno{annoSrcLoc, annoType = exp_ty}
      lit
tcRho t@(SynTerm'Var _ varName) exp_ty = do
  v_sigma <- lookupVar varName
  debug'
    "tcRho SynTerm'Var"
    [ ("varName", pretty' varName)
    , ("varName", prettyDetailed varName)
    , ("v_sigma", pretty' v_sigma)
    , ("v_sigma", prettyDetailed v_sigma)
    ,
      ( "exp_ty"
      , case exp_ty of
          Infer _ -> "Infer"
          Check t' -> "Check" <> line <> pretty' t'
      )
    , ("thing", pretty' (mkTypedThingIfCheck t exp_ty))
    ]
  instSigma (mkTypedThingIfCheck t exp_ty) v_sigma exp_ty
  pure $
    SynTerm'Var
      ()
      TcTermVar{varName, varType = exp_ty}
tcRho t@(SynTerm'App annoSrcLoc fun arg) exp_ty = do
  (fun', fun_ty) <- inferRho fun
  -- TODO should we pass Just here?
  (arg_ty, res_ty) <- unifyFun Nothing fun_ty
  arg' <- checkSigma arg arg_ty
  instSigma (mkTypedThingIfCheck t exp_ty) res_ty exp_ty
  pure $
    SynTerm'App
      TcAnno{annoSrcLoc, annoType = exp_ty}
      fun'
      arg'
tcRho t@(SynTerm'Lam annoSrcLoc varName body) modeTy@(Check exp_ty) = do
  (var_ty, body_ty) <- unifyFun (mkTypedThingIfCheck t modeTy) exp_ty
  debug'
    "tcRho SynTerm'Lam"
    [ ("varName", pretty' varName)
    , ("body", pretty' body)
    , ("exp_ty", pretty' exp_ty)
    ]
  body' <- extendVarEnv varName var_ty (checkRho body body_ty)
  pure $
    SynTerm'Lam
      TcAnno{annoSrcLoc, annoType = modeTy}
      TcTermVar{varName, varType = Check var_ty}
      body'
tcRho (SynTerm'Lam annoSrcLoc varName body) modeTy@(Infer ref) = do
  var_ty <- Type'Var <$> newMetaTyVar' "m"
  (body', body_ty) <- extendVarEnv varName var_ty (inferRho body)
  writeIORef ref (Type'Fun var_ty body_ty)
  pure $
    SynTerm'Lam
      TcAnno{annoSrcLoc, annoType = modeTy}
      TcTermVar{varName, varType = Check var_ty}
      body'
tcRho t@(SynTerm'ALam annoSrcLoc varName var_ty body) modeTy@(Check exp_ty) = do
  (arg_ty, body_ty) <- unifyFun (mkTypedThingIfCheck t modeTy) exp_ty
  (var_ty_syn, var_ty') <- convertSynTy var_ty
  -- TODO Should some typed thing be passed?
  subsCheck Nothing arg_ty var_ty'
  body' <- extendVarEnv varName var_ty' (checkRho body body_ty)
  pure $
    SynTerm'ALam
      TcAnno{annoSrcLoc, annoType = modeTy}
      TcTermVar{varName, varType = Check var_ty'}
      var_ty_syn
      body'
tcRho (SynTerm'ALam annoSrcLoc varName var_ty body) modeTy@(Infer ref) = do
  (var_ty_syn, var_ty') <- convertSynTy var_ty
  (body', body_ty) <- extendVarEnv varName var_ty' (inferRho body)
  writeIORef ref (Type'Fun var_ty' body_ty)
  -- TODO is it correct to use Check here?
  pure $
    SynTerm'ALam
      TcAnno{annoSrcLoc, annoType = modeTy}
      TcTermVar{varName, varType = Check var_ty'}
      var_ty_syn
      body'
tcRho (SynTerm'Let annoSrcLoc varName rhs body) exp_ty = do
  -- TODO call inferSigma here
  (rhs', var_ty) <- inferRho rhs
  body' <- extendVarEnv varName var_ty (tcRho body exp_ty)
  pure $
    SynTerm'Let
      TcAnno{annoType = exp_ty, annoSrcLoc}
      -- TODO is it correct to use Check here?
      TcTermVar{varName, varType = Check var_ty}
      rhs'
      body'
tcRho t@(SynTerm'Ann annoSrcLoc body ann_ty) exp_ty = do
  (ann_ty_syn, ann_ty') <- convertSynTy ann_ty
  debug'
    "tcRho SynTerm'Ann"
    [ ("body", pretty' body)
    , ("ann_ty", pretty' ann_ty)
    , ("ann_ty'", pretty' ann_ty')
    ]
  body' <- checkSigma body ann_ty'
  instSigma (mkTypedThingIfCheck t exp_ty) ann_ty' exp_ty
  pure $
    SynTerm'Ann
      TcAnno{annoSrcLoc, annoType = exp_ty}
      body'
      ann_ty_syn

-- | Similar to @tcInferSigma@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Gen/App.hs#L174
inferSigma :: SynTerm CompRn -> TcM (SynTerm CompTc, Sigma)
inferSigma = error "Not implemented!"

-- ==============================================
-- Capture constraints
-- ==============================================

-- | Capture constraints at a deeper level.
--
-- Similar to @pushLevelAndCaptureConstraints@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Monad.hs#L1909
pushLevelAndCaptureConstraints ::
  -- | skolems
  [TcTyVar] ->
  TcM a ->
  TcM a
pushLevelAndCaptureConstraints skol_tvs act = do
  constraints <- newIORef emptyWantedConstraints

  let tcLevelNew = ?tcLevel + fromInteger 1

  expr' <- do
    let ?constraints = constraints
        ?tcLevel = tcLevelNew
     in act

  constraintsSkol' <- readIORef constraints

  let implicationCt =
        Implic
          { ic_tclvl = tcLevelNew
          , ic_skols = skol_tvs
          , ic_env = Nothing
          , ic_wanted = constraintsSkol'
          , ic_status = IC_Unsolved
          }

  constraintsLocal <- readIORef ?constraints

  let constraintsLocal' =
        constraintsLocal
          { wc_impl = unitBag implicationCt <> constraintsLocal.wc_impl
          }

  writeIORef ?constraints constraintsLocal'

  pure expr'

-- ==============================================
-- checkSigma
-- ==============================================

-- | Similar to @tcCheckPolyLExpr@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Gen/Expr.hs#L115
checkSigma :: SynTerm CompRn -> Sigma -> TcM (SynTerm CompTc)
checkSigma expr sigma =
  do
    debug'
      "checkSigma before skolemise"
      [ ("sigma", pretty' sigma)
      ]
    (skol_tvs, rho) <- skolemise sigma
    debug'
      "checkSigma after skolemise"
      [ ("skol_tvs", pretty' skol_tvs)
      , ("rho", pretty' rho)
      ]

    -- Skolem escape will be checked in the solver
    -- using levels of type variables.

    -- > implication constraints check for the skolem escape.
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Gen/Bind.hs#L1774

    -- Skolemisation builds an implication constraint.
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L372
    --
    -- Also see 'buildImplicationFor' in GHC
    --

    -- Now:
    -- - emit an implication constraint with skolems
    -- - collect other constraints
    -- - put them into that implication constraint as wanteds

    pushLevelAndCaptureConstraints skol_tvs (checkRho expr rho)

-- ==============================================
-- Subsumption checking
-- ==============================================

subsCheck :: Maybe TypedThing -> Sigma -> Sigma -> TcM ()
subsCheck thing sigma1 sigma2 = do
  -- Rule DEEP-SKOL from the paper.
  (skol_tvs, rho2) <- skolemise sigma2
  pushLevelAndCaptureConstraints skol_tvs (subsCheckRho thing sigma1 rho2)

subsCheckRho :: Maybe TypedThing -> Sigma -> Rho -> TcM ()
-- Invariant: the third argument is in weak-prenex form

-- Rule SPEC
subsCheckRho thing sigma1@(Type'ForAll _ _) rho2 = do
  rho1 <- instantiate sigma1
  subsCheckRho thing rho1 rho2
-- Rule FUN
subsCheckRho thing rho1 (Type'Fun a2 r2) = do
  (a1, r1) <- unifyFun thing rho1
  -- `unifyFun` already called unify with `thing`
  subsCheckFun Nothing a1 r1 a2 r2
-- Rule FUN
subsCheckRho thing (Type'Fun a1 r1) rho2 = do
  (a2, r2) <- unifyFun thing rho2
  -- `unifyFun` already called unify with `thing`
  subsCheckFun Nothing a1 r1 a2 r2
-- Rule MONO
subsCheckRho thing tau1 tau2 = do
  -- Revert to ordinary unification
  debug'
    "subsCheckRho"
    [ ("thing", pretty' thing)
    , ("tau1", pretty' tau1)
    , ("tau2", pretty' tau2)
    ]
  unify thing tau1 tau2

subsCheckFun :: Maybe TypedThing -> Sigma -> Rho -> Sigma -> Rho -> TcM ()
subsCheckFun _thing a1 r1 a2 r2 = do
  -- TODO argument order
  -- unify with _thing must be called somewhere before
  subsCheck Nothing a2 a1
  subsCheckRho Nothing r1 r2

instSigma :: Maybe TypedThing -> Sigma -> Expected Rho -> TcM ()
-- Invariant: if the second argument is (Check rho),
--            then rho is in weak-prenex form
instSigma thing tyActual (Check tyExpected) = do
  debug'
    "instSigma Check"
    [ ("thing", pretty' thing)
    , ("tyActual", pretty' tyActual)
    , ("tyExpected", pretty' tyExpected)
    ]
  withTcError
    TcError'UnexpectedType
      { thing
      , expected = tyExpected
      , actual = tyActual
      }
    $ subsCheckRho thing tyActual tyExpected
instSigma thing sigma (Infer tyExpected) = do
  debug'
    "instSigma Infer before instantiate"
    [ ("thing", pretty' thing)
    , ("sigma", pretty' sigma)
    ]
  rho <- instantiate sigma
  debug'
    "instSigma Infer after instantiate"
    [ ("thing", pretty' thing)
    , ("sigma", pretty' sigma)
    , ("rho", pretty' rho)
    ]
  writeIORef tyExpected rho
