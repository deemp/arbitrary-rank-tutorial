{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Typechecker.TcMonad where

import Data.IORef (readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Traversable (forM)
import GHC.Stack (HasCallStack)
import Language.Arralac.Prelude.Bag
import Language.Arralac.Prelude.Debug
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Prelude.Unique
import Language.Arralac.Solver.Types
import Language.Arralac.Type.Local.TyVar.Tc
import Language.Arralac.Type.Local.Type
import Language.Arralac.Type.TTG.Type
import Language.Arralac.Typechecker.Constraints
import Language.Arralac.Typechecker.Error
import Language.Arralac.Typechecker.TcTyVar (isBoundTvTypeVar, newMetaTyVar', newSkolemTyVar, tyVarToMetaTyVar)
import Language.Arralac.Typechecker.TcTyVarEnv
import Language.Arralac.Typechecker.Types

-- =========================
-- [The Type Checking Monad]
-- =========================

type CtxTcEnv =
  ( HasCallStack
  , CtxUniqueSupply
  , CtxPrettyVerbosity
  , CtxDebug
  , CtxTcTyVarEnv
  , CtxTcLevel
  , CtxWantedConstraints
  , CtxTcErrorPropagated
  , CtxSolverIterations
  )

-- TcM in GHC is a ReaderT (Env a) IO b.
-- It can be replaced with ImplicitParams
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types.hs#L271
type TcM a = (CtxTcEnv) => IO a

-- ===============
-- [Instantiation]
-- ===============

-- | Instantiate type variables in the topmost 'forall'
-- of the argument type with 'Flexi' metavariables.
instantiate :: Sigma -> TcM Rho
instantiate (Type'ForAll tvs ty) = do
  tvs' <- forM tvs tyVarToMetaTyVar
  debug'
    "instantiate Type'ForAll"
    [ ("tvs", pretty' tvs)
    , ("ty", pretty' ty)
    , ("tvs'", pretty' tvs')
    ]
  substTy tvs (Type'Var <$> tvs') ty
instantiate ty = pure ty

-- ===============
-- [Skolemisation]
-- ===============

-- | Performs deep skolemisation, returning the
-- skolem constants and the skolemised type.
--
-- Similar to @topSkolemise@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Instantiate.hs#L201
--
-- See Note [When to build an implication] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L628
--
-- See Note [Skolemisation overview] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L282
skolemise :: Sigma -> TcM ([TcTyVar], Rho)
-- Rule PRPOLY
skolemise (Type'ForAll tvs ty) = do
  -- TODO Do we need recursive do?
  -- Note [Keeping SkolemInfo inside a SkolemTv]
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L579
  let info = ForAllSkol tvs
  sks1 <- mapM (newSkolemTyVar info) tvs
  (sks2, ty') <- skolemise =<< (substTy tvs (Type'Var <$> sks1) ty)
  pure (sks1 <> sks2, ty')
-- Rule PRFUN
skolemise (Type'Fun arg_ty res_ty) = do
  (sks, res_ty') <- skolemise res_ty
  pure (sks, Type'Fun arg_ty res_ty')
-- Rule PRMONO
skolemise ty = pure ([], ty)

-- ==============
-- [Substitution]
-- ==============

-- | Replace the specified quantified (introduced in a 'forall') type variables by given meta type variables.
--
-- No worries about capture, because the two kinds of type variable are distinct.
substTy :: [TcBoundVar] -> [TcTypeMeta] -> TcType -> TcM TcType
substTy tvs tys ty = subst_ty (Map.fromList (tvs `zip` tys)) ty

type SubstitutionEnv = Map.Map TcBoundVar TcType

subst_ty :: SubstitutionEnv -> TcType -> TcM TcType
subst_ty env (Type'Fun arg res) =
  Type'Fun <$> (subst_ty env arg) <*> (subst_ty env res)
subst_ty env (Type'Var n@TcTyVar{varDetails = BoundTv{}}) = do
  let res = fromMaybe (Type'Var n) (Map.lookup n env)
  debug'
    "subst_ty Type'Var BoundTv"
    [ ("env", pretty' (Map.toAscList env))
    , ("res", pretty' res)
    ]
  pure res
subst_ty _ (Type'Var n@TcTyVar{varDetails = MetaTv{}}) =
  -- Just like in the paper
  pure $ Type'Var n
-- TODO should we panic if skolem?
subst_ty _ (Type'Var var@TcTyVar{}) =
  dieTc TcError'UnboundVariable{var}
subst_ty _ (Type'Concrete tc) =
  pure $ Type'Concrete tc
subst_ty env (Type'ForAll ns rho) = do
  let ns' = Set.fromList ns
      -- TODO works correctly?
      env' = Map.withoutKeys env ns'
  -- It's not skolemisation
  -- so we can leave these variables as they are now
  Type'ForAll ns <$> (subst_ty env' rho)

-- =============
-- [Unification]
-- =============

-- TODO ^ use only necessary constraints?

-- | Extends the substitution by side effect (p. 43)
--
-- Similar to @unifyType@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L2011
--
-- See Note [Unification preconditions]:
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L2589
unify :: Maybe TypedThing -> Tau -> Tau -> TcM ()
-- Invariant:
-- The first type is "expected",
-- the second one is "actual"
unify thing ty1 ty2
  | isBoundTvTypeVar ty1 || isBoundTvTypeVar ty2 -- Compiler error
    =
      do
        debug'
          "unify bound types"
          [ ("?tcTyVarEnv", prettyDetailed (toAscListTcTyVarEnv ?tcTyVarEnv))
          ]
        dieTc (TcError'UnifyingBoundTypes ty1 ty2 thing)
unify
  _thing
  (Type'Var TcTyVar{varDetails = MetaTv{metaTvRef = tv1}})
  (Type'Var TcTyVar{varDetails = MetaTv{metaTvRef = tv2}})
    | tv1 == tv2 = pure ()
unify thing (Type'Var tv@TcTyVar{varDetails = MetaTv{}}) ty =
  unifyVar thing tv ty False
unify thing ty (Type'Var tv@TcTyVar{varDetails = MetaTv{}}) =
  -- We have to let unifyVar know that we swapped "expected" and "actual"
  unifyVar thing tv ty True
-- TODO is equality defined correctly?
unify _thing (Type'Var tv1@TcTyVar{}) (Type'Var tv2@TcTyVar{})
  | tv1 == tv2 = pure ()
unify _thing (Type'Fun arg1 res1) (Type'Fun arg2 res2) = do
  -- TODO should we inspect thing?
  unify Nothing arg1 arg2
  unify Nothing res1 res2
unify _thing (Type'Concrete tc1) (Type'Concrete tc2)
  | tc1 == tc2 = pure ()
unify thing ty1 ty2 =
  dieTc TcError'CannotUnify{ty1 = ty1, ty2 = ty2, thing}

-- | Unify a variable with a type.
--
-- The 'Maybe TypedThing' may provide evidence for this equality
unifyVar :: Maybe TypedThing -> TcTyVar -> Tau -> Bool -> TcM ()
-- Invariant: tv is a flexible type variable
-- Check whether tv is bound
unifyVar thing tv@TcTyVar{varDetails = MetaTv{}} ty swapped = do
  emitCEqCan thing tv ty swapped
unifyVar _thing _tv _ty _ = pure ()

-- | Unify a Rho-type with a function type a[Meta] -> b[Meta]
-- that has metavariables as its argument and result.
unifyFun :: Maybe TypedThing -> Rho -> TcM (Sigma, Rho)
-- unifies 'fun' with '(arg -> res)'
unifyFun _thing (Type'Fun arg res) = pure (arg, res)
unifyFun thing tau = do
  -- TODO use better names?
  arg_ty <- Type'Var <$> newMetaTyVar' "a"
  res_ty <- Type'Var <$> newMetaTyVar' "b"
  unify thing tau (Type'Fun arg_ty res_ty)
  pure (arg_ty, res_ty)

emitCEqCan :: Maybe TypedThing -> TcTyVar -> Tau -> Bool -> TcM ()
emitCEqCan thing tv ty swapped = do
  constraintsCur <- readIORef ?constraints
  let constraint = mkCEqCan thing tv ty swapped
      constraintsNew =
        constraintsCur
          { wc_simple = Bag [constraint] <> constraintsCur.wc_simple
          }
  writeIORef ?constraints constraintsNew

-- ================
-- [Quantification]
-- ================

-- | Solve constraints and quantify over type variables.
--
-- Similar to @simplifyInfer@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Solver.hs#L878
--
-- Note [quantifyTyVars] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1677
--
-- Note [Deciding quantification] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Solver.hs#L1138
quantify :: TcLevel -> [TcTyVarMeta] -> Rho -> WantedConstraints -> TcM Sigma
-- Quantify over the specified type variables (all flexible)
quantify = error "Not implemented!"

-- TODO ^ implement a zonker to be used during typechecking.
--
-- See Note [What is zonking?] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Zonk/Type.hs#L106

-- TODO ^ Should 'quantify' use 'BoundTv' in forall?
--
-- See "TyVarDetails, MetaDetails, MetaInfo" in GHC
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L549
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L566
--
-- As written in Sec. 5.5 of Practical type inference for arbitrary-rank types:
--
-- > Furthermore, quantify guarantees to return a type that is fully substituted;
-- this makes it easier to instantiate later, because the proper type variables
-- can all be found without involving the substitution.
--
-- So, perhaps yes, it should use 'BoundTv'.
