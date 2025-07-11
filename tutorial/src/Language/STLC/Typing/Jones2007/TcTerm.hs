module Language.STLC.Typing.Jones2007.TcTerm where

import Data.Foldable (Foldable (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Language.STLC.Typing.Jones2007.Bag (unitBag)
import Language.STLC.Typing.Jones2007.BasicTypes
import Language.STLC.Typing.Jones2007.BasicTypes qualified as BT
import Language.STLC.Typing.Jones2007.Constraints (ImplicStatus (..), Implication (..), TypedThing (..), WantedConstraints (..), emptyWantedConstraints)
import Language.STLC.Typing.Jones2007.Solver (Solve (..), toListWc)
import Language.STLC.Typing.Jones2007.TcMonad
import Language.STLC.Typing.Renamer (parseInputText)
import Language.STLC.Typing.Zonker (Zonk (..))
import Prettyprinter (line, (<+>))
import UnliftIO.Exception (finally)

-- ==============================================
--      The top-level wrapper
-- ==============================================

-- TODO Too much boilerplate...

-- TODO report many independent errors, not fail on the first error
typecheck :: SynTerm CompRn -> TcM (SynTerm CompZn)
typecheck term =
  do
    tcResult <- inferSigma term
    zonk (fst tcResult)
    `finally` do
      constraints <- readIORef ?constraints
      debug'
        "typecheck"
        [ ("constraints", pretty' constraints)
        ]

runTypechecker :: (IDebug, IScope, IUniqueSupply, ICurrentFilePath) => SynTerm BT.CompRn -> IO (SynTerm BT.CompZn)
runTypechecker program = do
  constraints <-
    newIORef
      WantedCts
        { wc_simple = emptyBag
        , wc_impl = emptyBag
        }
  tcError <- newIORef Nothing
  let
    ?tcLevel = BT.TcLevel 0
    ?varEnv = Map.empty
    ?constraints = constraints
    ?tcErrorPropagated = tcError
    ?debug = ?debug
    ?scope = ?scope
    ?uniqueSupply = ?uniqueSupply
    ?currentFilePath = ?currentFilePath
  typecheck program
  
-- TODO use filepath from implicit params
runTypechecker' :: (HasCallStack, BT.IDebug, IPrettyVerbosity) => BT.FastString -> T.Text -> IO (SynTerm CompZn)
runTypechecker' filePath content = do
  uniqueSupply <- newIORef 0
  let ?uniqueSupply = uniqueSupply
      ?currentFilePath = filePath
      ?scope = Map.empty
      ?debug = ?debug
      ?callStack = ?callStack
  program <- parseInputText content
  runTypechecker program

-- ==============================================
-- tcRho and its variants
-- ==============================================

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

-- TODO optimize
parseTypeConcrete :: Name -> TcM TypeConcrete
parseTypeConcrete name = do
  let name' = T.unpack name.nameOcc.occNameFS
  if
    | name' == show TypeConcrete'Bool -> pure TypeConcrete'Bool
    | name' == show TypeConcrete'String -> pure TypeConcrete'String
    | name' == show TypeConcrete'Int -> pure TypeConcrete'Int
    | otherwise -> die TcError'UnknownConcreteType{name}

-- TODO Levels
convertSynTy :: SynType CompRn -> TcM (SynType CompTc, TcType)
convertSynTy = \case
  SynType'Var _ var -> do
    -- TODO correct tcLevel?
    var' <- pure TcTyVar{varName = var, varDetails = BoundTv{tcLevel = ?tcLevel}}
    pure $ (SynType'Var () var', Type'Var var')
  SynType'ForAll srcLoc tvs body -> do
    -- TODO correct tcLevel?
    let tvs' = (\varName -> TcTyVar{varName, varDetails = BoundTv{tcLevel = ?tcLevel}}) <$> tvs
    (body', body'ty) <- convertSynTy body
    pure $ (SynType'ForAll srcLoc tvs' body', Type'ForAll tvs' body'ty)
  SynType'Fun srcLoc arg res -> do
    (arg', arg'ty) <- convertSynTy arg
    (res', res'ty) <- convertSynTy res
    pure $ (SynType'Fun srcLoc arg' res', Type'Fun arg'ty res'ty)
  -- TODO handle more correctly?
  SynType'Concrete srcLoc lit -> do
    concreteType <- parseTypeConcrete lit.nameOcc.occNameFS
    pure $ (SynType'Concrete srcLoc Concrete{concreteName = lit, concreteType}, Type'Concrete concreteType)

-- TODO should we provide typed thing during inference?
mkTypedThingIfCheck :: SynTerm CompRn -> Expected a -> Maybe TypedThing
mkTypedThingIfCheck thing = \case
  Infer _ -> Nothing
  Check _ -> Just (HsExprRnThing thing)

-- | Similar to `tcCheckPolyExpr` in GHC.
-- 
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
    , ("varName", prettyVerbose varName)
    , ("v_sigma", pretty' v_sigma)
    , ("v_sigma", prettyVerbose v_sigma)
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

-- ==============================================
-- inferSigma and checkSigma
-- ==============================================

annotateTc :: Sigma -> SynTerm CompTc -> SynTerm CompTc
annotateTc s = \case
  SynTerm'Var anno v ->
    SynTerm'Var anno v
  SynTerm'Lit anno l ->
    SynTerm'Lit anno l
  SynTerm'App TcAnno{annoSrcLoc} arg res ->
    SynTerm'App TcAnno{annoSrcLoc, annoType = Check s} arg res
  SynTerm'Lam TcAnno{annoSrcLoc} var body ->
    SynTerm'Lam TcAnno{annoSrcLoc, annoType = Check s} var body
  SynTerm'ALam TcAnno{annoSrcLoc} var ty body ->
    SynTerm'ALam TcAnno{annoSrcLoc, annoType = Check s} var ty body
  SynTerm'Let TcAnno{annoSrcLoc} var val term ->
    SynTerm'Let TcAnno{annoSrcLoc, annoType = Check s} var val term
  SynTerm'Ann TcAnno{annoSrcLoc} body ty ->
    SynTerm'Ann TcAnno{annoSrcLoc, annoType = Check s} body ty

inferSigma :: SynTerm CompRn -> TcM (SynTerm CompTc, Sigma)
inferSigma e =
  do
    (e', exp_ty) <- inferRho e
    env_tys <- getEnvTypes
    env_tvs <- getMetaTyVars env_tys
    res_tvs <- getMetaTyVars [exp_ty]
    let forall_tvs = res_tvs \\ env_tvs
    debug'
      "inferSigma"
      [ "env_tvs (pretty):"
      , pretty env_tvs
      , "res_tvs (pretty):"
      , pretty res_tvs
      , "forall_tvs (pretty):"
      , pretty forall_tvs
      , "exp_ty (pretty): "
      , pretty exp_ty
      , "exp_ty"
      , pretty (show exp_ty)
      ]
    ty' <- quantify forall_tvs exp_ty
    let e'' = annotateTc ty' e'
    pure (e'', ty')

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
--      Subsumption checking            --
------------------------------------------

subsCheck :: Maybe TypedThing -> Sigma -> Sigma -> TcM ()
-- (subsCheck args off exp) checks that
--     'off' is at least as polymorphic as 'args -> exp'

-- Rule DEEP-SKOL
subsCheck thing sigma1 sigma2 = do
  (skol_tvs, rho2) <- skolemise sigma2
  subsCheckRho thing sigma1 rho2
  esc_tvs <- getFreeTyVars [sigma1, sigma2]
  -- TODO levels?
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

subsCheckRho :: Maybe TypedThing -> Sigma -> Rho -> TcM ()
-- Invariant: the second argument is in weak-prenex form

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
