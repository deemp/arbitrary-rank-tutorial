{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.STLC.Typing.Jones2007.TcTerm where

import Control.Monad (forM)
import Data.List ((\\))
import Data.Text (Text)
import Data.Text qualified as T
import Language.STLC.Typing.Jones2007.BasicTypes
import Language.STLC.Typing.Jones2007.TcMonad
import Prettyprinter

------------------------------------------
--      The top-level wrapper           --
------------------------------------------

-- TODO Too much boilerplate...

-- TODO report many independent errors, not fail on the first error
typecheck :: SynTerm CompRn -> TcM (SynTerm CompZn)
typecheck e = do
  (e'tc, _) <- inferSigma e
  zonkFinallyTerm e'tc

zonkFinallySynType :: SynType CompTc -> TcM (SynType CompZn)
zonkFinallySynType = \case
  SynType'Var anno TcTyVar{varName} -> do
    pure $ SynType'Var anno ZnTyVar{varName}
  SynType'ForAll srcLoc vars body -> do
    vars' <- forM vars zonkFinallyTcTyVar
    body' <- zonkFinallySynType body
    pure $ SynType'ForAll srcLoc vars' body'
  SynType'Fun srcLoc arg res -> do
    arg' <- zonkFinallySynType arg
    res' <- zonkFinallySynType res
    pure $ SynType'Fun srcLoc arg' res'
  SynType'Paren srcLoc body -> do
    body' <- zonkFinallySynType body
    pure $ SynType'Paren srcLoc body'
  SynType'Concrete anno lit -> do
    pure $ SynType'Concrete anno lit

zonkFinallyTcTyVar :: TcTyVar -> TcM ZnTyVar
zonkFinallyTcTyVar TcTyVar{varName} =
  pure ZnTyVar{varName}

-- TODO are zonks of different parts of a type independent?
zonkFinallyTcType :: TcType -> TcM ZnType
zonkFinallyTcType = \case
  Type'Var TcTyVar{varName} -> do
    pure $ Type'Var ZnTyVar{varName}
  Type'ForAll vars body -> do
    vars' <- forM vars zonkFinallyTcTyVar
    body' <- zonkFinallyTcType body
    pure $ Type'ForAll vars' body'
  Type'Fun arg res -> do
    arg' <- zonkFinallyTcType arg
    res' <- zonkFinallyTcType res
    pure $ Type'Fun arg' res'
  Type'Concrete ty -> do
    pure $ Type'Concrete ty

zonkFinallyAnno :: AnnoTc -> TcM AnnoZn
zonkFinallyAnno AnnoTc{annoSrcLoc, annoType} = do
  annoType' <- zonkFinallyExpectedType annoType
  pure AnnoZn{annoSrcLoc, annoType = annoType'}

zonkFinallyTcTermVar :: TcTermVar -> TcM ZnTermVar
zonkFinallyTcTermVar TcTermVar{varName, varType} = do
  varType' <- zonkFinallyExpectedType varType
  pure ZnTermVar{varName, varType = varType'}

zonkFinallyExpectedType :: Expected TcType -> TcM ZnType
zonkFinallyExpectedType ty = do
  varType' <-
    case ty of
      Infer r -> readTcRef r
      Check t -> pure t
  zonkFinallyTcType varType'

zonkFinallyTerm :: SynTerm CompTc -> TcM (SynTerm CompZn)
zonkFinallyTerm = \case
  SynTerm'Var _ var -> do
    var' <- zonkFinallyTcTermVar var
    pure $ SynTerm'Var () var'
  SynTerm'Lit ann lit -> do
    pure $ SynTerm'Lit ann lit
  SynTerm'App anno fun arg -> do
    anno' <- zonkFinallyAnno anno
    fun' <- zonkFinallyTerm fun
    arg' <- zonkFinallyTerm arg
    pure $ SynTerm'App anno' fun' arg'
  SynTerm'Lam anno var ty -> do
    anno' <- zonkFinallyAnno anno
    var' <- zonkFinallyTcTermVar var
    ty' <- zonkFinallyTerm ty
    pure $ SynTerm'Lam anno' var' ty'
  SynTerm'ALam anno var ty body -> do
    anno' <- zonkFinallyAnno anno
    var' <- zonkFinallyTcTermVar var
    ty' <- zonkFinallySynType ty
    body' <- zonkFinallyTerm body
    pure $ SynTerm'ALam anno' var' ty' body'
  SynTerm'Let anno var val term -> do
    anno' <- zonkFinallyAnno anno
    var' <- zonkFinallyTcTermVar var
    val' <- zonkFinallyTerm val
    term' <- zonkFinallyTerm term
    pure $ SynTerm'Let anno' var' val' term'
  SynTerm'Ann anno term ty -> do
    anno' <- zonkFinallyAnno anno
    term' <- zonkFinallyTerm term
    ty' <- zonkFinallySynType ty
    pure $ SynTerm'Ann anno' term' ty'

-----------------------------------
--      The expected type       --
-----------------------------------

------------------------------------------
--      tcRho, and its variants         --
------------------------------------------

checkRho :: SynTerm CompRn -> Rho -> TcM (SynTerm CompTc)
-- Invariant: the Rho is always in weak-prenex form
checkRho expr ty = tcRho expr (Check ty)

-- TODO should return synterm along with the type?
inferRho :: SynTerm CompRn -> TcM (SynTerm CompTc, Rho)
inferRho expr =
  do
    debug'
      "inferRho"
      [pretty expr]
    ref <- newTcRef (error "inferRho: empty result")
    expr' <- tcRho expr (Infer ref)
    ref' <- readTcRef ref
    pure (expr', ref')

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

parseTypeConcrete :: Text -> TcM TypeConcrete
parseTypeConcrete name
  | name == T.pack (show TypeConcrete'Bool) = pure TypeConcrete'Bool
  | name == T.pack (show TypeConcrete'String) = pure TypeConcrete'String
  | name == T.pack (show TypeConcrete'Int) = pure TypeConcrete'Int
  | otherwise = die ("Unknown concrete type: " <> pretty name)

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
  SynType'Paren srcLoc ty -> do
    (ty', ty'ty) <- convertSynTy ty
    pure $ (SynType'Paren srcLoc ty', ty'ty)
  SynType'Concrete srcLoc lit -> do
    concreteType <- parseTypeConcrete lit.nameOcc.occNameFS
    pure $ (SynType'Concrete srcLoc Concrete{concreteName = lit, concreteType}, Type'Concrete concreteType)

-- TODO return reconstructed SynTerm CompTc
tcRho :: SynTerm CompRn -> Expected Rho -> TcM (SynTerm CompTc)
-- Invariant: if the second argument is (Check rho),
--            then rho is in weak-prenex form
tcRho (SynTerm'Lit _ lit) exp_ty = do
  let ty =
        case lit of
          SynLit'Num{} -> TypeConcrete'Int
          SynLit'Bool{} -> TypeConcrete'Bool
          SynLit'Str{} -> TypeConcrete'String
  instSigma (Type'Concrete ty) exp_ty
  -- TODO what to return?
  -- TODO should we use the annotation field
  -- to store the inferred type?
  pure $
    SynTerm'Lit
      ty
      lit
tcRho (SynTerm'Var _ varName) exp_ty = do
  v_sigma <- lookupVar varName
  debug'
    "var"
    [ pretty varName
    , pretty (show varName)
    , pretty v_sigma
    , pretty (show v_sigma)
    , case exp_ty of
        Infer _ -> "Infer"
        Check t -> "Check" <> pretty (show t)
    ]
  instSigma v_sigma exp_ty
  pure $
    SynTerm'Var
      ()
      TcTermVar{varName, varType = exp_ty}
tcRho (SynTerm'App annoSrcLoc fun arg) exp_ty = do
  (fun', fun_ty) <- inferRho fun
  (arg_ty, res_ty) <- unifyFun fun_ty
  arg' <- checkSigma arg arg_ty
  instSigma res_ty exp_ty
  pure $
    SynTerm'App
      AnnoTc{annoSrcLoc, annoType = exp_ty}
      fun'
      arg'
tcRho (SynTerm'Lam annoSrcLoc varName body) modeTy@(Check exp_ty) = do
  (var_ty, body_ty) <- unifyFun exp_ty
  debug'
    "lam"
    [ pretty varName
    , pretty body
    , pretty exp_ty <+> pretty (var_ty, body_ty)
    ]
  body' <- extendVarEnv varName var_ty (checkRho body body_ty)
  pure $
    SynTerm'Lam
      AnnoTc{annoSrcLoc, annoType = modeTy}
      TcTermVar{varName, varType = Check var_ty}
      body'
tcRho (SynTerm'Lam annoSrcLoc varName body) modeTy@(Infer ref) = do
  var_ty <- Type'Var <$> newMetaTyVar' "m"
  (body', body_ty) <- extendVarEnv varName var_ty (inferRho body)
  writeTcRef ref (var_ty --> body_ty)
  pure $
    SynTerm'Lam
      AnnoTc{annoSrcLoc, annoType = modeTy}
      TcTermVar{varName, varType = Check var_ty}
      body'
tcRho (SynTerm'ALam annoSrcLoc varName var_ty body) modeTy@(Check exp_ty) = do
  (arg_ty, body_ty) <- unifyFun exp_ty
  (var_ty_syn, var_ty') <- convertSynTy var_ty
  subsCheck arg_ty var_ty'
  body' <- extendVarEnv varName var_ty' (checkRho body body_ty)
  pure $
    SynTerm'ALam
      AnnoTc{annoSrcLoc, annoType = modeTy}
      TcTermVar{varName, varType = Check var_ty'}
      var_ty_syn
      body'
tcRho (SynTerm'ALam annoSrcLoc varName var_ty body) modeTy@(Infer ref) = do
  (var_ty_syn, var_ty') <- convertSynTy var_ty
  (body', body_ty) <- extendVarEnv varName var_ty' (inferRho body)
  writeTcRef ref (var_ty' --> body_ty)
  -- TODO is it correct to use Check here?
  pure $
    SynTerm'ALam
      AnnoTc{annoSrcLoc, annoType = modeTy}
      TcTermVar{varName, varType = Check var_ty'}
      var_ty_syn
      body'
tcRho (SynTerm'Let annoSrcLoc varName rhs body) exp_ty = do
  (rhs', var_ty) <- inferSigma rhs
  body' <- extendVarEnv varName var_ty (tcRho body exp_ty)
  pure $
    SynTerm'Let
      AnnoTc{annoType = exp_ty, annoSrcLoc}
      -- TODO is it correct to use Check here?
      TcTermVar{varName, varType = Check var_ty}
      rhs'
      body'
tcRho (SynTerm'Ann annoSrcLoc body ann_ty) exp_ty = do
  (ann_ty_syn, ann_ty') <- convertSynTy ann_ty
  debug'
    "tcRho Ann"
    [ "body (pretty):"
    , pretty body
    , "ann_ty (pretty):"
    , pretty ann_ty
    , "ann_ty' (pretty)"
    , pretty ann_ty'
    ]
  body' <- checkSigma body ann_ty'
  instSigma ann_ty' exp_ty
  pure $
    SynTerm'Ann
      AnnoTc{annoSrcLoc, annoType = exp_ty}
      body'
      ann_ty_syn

------------------------------------------
--      inferSigma and checkSigma
------------------------------------------

annotateTc :: Sigma -> SynTerm CompTc -> SynTerm CompTc
annotateTc s = \case
  SynTerm'Var anno v ->
    SynTerm'Var anno v
  SynTerm'Lit anno l ->
    SynTerm'Lit anno l
  SynTerm'App AnnoTc{annoSrcLoc} arg res ->
    SynTerm'App AnnoTc{annoSrcLoc, annoType = Check s} arg res
  SynTerm'Lam AnnoTc{annoSrcLoc} var body ->
    SynTerm'Lam AnnoTc{annoSrcLoc, annoType = Check s} var body
  SynTerm'ALam AnnoTc{annoSrcLoc} var ty body ->
    SynTerm'ALam AnnoTc{annoSrcLoc, annoType = Check s} var ty body
  SynTerm'Let AnnoTc{annoSrcLoc} var val term ->
    SynTerm'Let AnnoTc{annoSrcLoc, annoType = Check s} var val term
  SynTerm'Ann AnnoTc{annoSrcLoc} body ty ->
    SynTerm'Ann AnnoTc{annoSrcLoc, annoType = Check s} body ty

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
      [ "sigma (pretty):"
      , pretty sigma
      ]
    (skol_tvs, rho) <- skolemise sigma
    debug'
      "checkSigma after skolemise"
      [ "skol_tvs (pretty):"
      , pretty skol_tvs
      , "rho (pretty):"
      , pretty rho
      ]
    expr' <- checkRho expr rho
    env_tys <- getEnvTypes
    esc_tvs <- getFreeTyVars (sigma : env_tys)
    let bad_tvs = filter (`elem` esc_tvs) skol_tvs
    check
      (null bad_tvs)
      ("Type not polymorphic enough")
    pure expr'

------------------------------------------
--      Subsumption checking            --
------------------------------------------

subsCheck :: Sigma -> Sigma -> TcM ()
-- (subsCheck args off exp) checks that
--     'off' is at least as polymorphic as 'args -> exp'

-- Rule DEEP-SKOL
subsCheck sigma1 sigma2 = do
  (skol_tvs, rho2) <- skolemise sigma2
  subsCheckRho sigma1 rho2
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

subsCheckRho :: Sigma -> Rho -> TcM ()
-- Invariant: the second argument is in weak-prenex form

-- Rule SPEC
subsCheckRho sigma1@(Type'ForAll _ _) rho2 = do
  rho1 <- instantiate sigma1
  subsCheckRho rho1 rho2
-- Rule FUN
subsCheckRho rho1 (Type'Fun a2 r2) = do
  (a1, r1) <- unifyFun rho1
  subsCheckFun a1 r1 a2 r2
-- Rule FUN
subsCheckRho (Type'Fun a1 r1) rho2 = do
  (a2, r2) <- unifyFun rho2
  subsCheckFun a1 r1 a2 r2
-- Rule MONO
subsCheckRho tau1 tau2 =
  -- Revert to ordinary unification
  unify tau1 tau2

subsCheckFun :: Sigma -> Rho -> Sigma -> Rho -> TcM ()
subsCheckFun a1 r1 a2 r2 = do
  subsCheck a2 a1
  subsCheckRho r1 r2

instSigma :: Sigma -> Expected Rho -> TcM ()
-- Invariant: if the second argument is (Check rho),
--            then rho is in weak-prenex form
instSigma t1 (Check t2) = subsCheckRho t1 t2
instSigma t1 (Infer r) = do
  t1' <- instantiate t1
  writeTcRef r t1'
