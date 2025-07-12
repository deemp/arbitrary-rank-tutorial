{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Typechecker.Solver where

import Control.Exception (Exception, throw)
import Control.Monad (forM, forM_, when)
import Data.Foldable (Foldable (..))
import Data.IORef (readIORef, writeIORef)
import Data.Set qualified as Set
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.Type
import Language.Arralac.Typechecker.TcMonad (badType, debug')
import Language.Arralac.Typechecker.Types.Constraints
import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types
import Language.Arralac.Utils.Types.Bag
import Prettyprinter ((<+>))

-- Constraint solver.
--
-- Limitations:
--
-- No promotion. See Note [Promotion and level-checking] in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L3588

type SolveM a = (CtxDebug, CtxPrettyVerbosity) => IO a

type CtxLhsMetaTv = (?lhsMetaTv :: TcTyVar)

type CtxMetaTvScope = (?metaTvScope :: Set.Set TcTyVar)

type CtxCt = (?ct :: Ct)

class Solve a where
  type SolveTo a
  solve :: (CtxMetaTvScope) => a -> SolveM (SolveTo a)

instance Solve Ct where
  type SolveTo Ct = [Ct]
  solve ct = do
    let lhs = ct.ct_eq_can.eq_lhs
        rhs = ct.ct_eq_can.eq_rhs
    case lhs.varDetails of
      -- TODO why can we get not a metavar on LHS?
      SkolemTv{} -> dieSolver SolverError'CannotUnifySkolemVar{ct}
      BoundTv{} -> dieSolver SolverError'CannotUnifyBoundVar{tv = lhs, ty = rhs, ct}
      MetaTv{metaTvRef} -> do
        let ?lhsMetaTv = lhs
            ?metaTvScope = Set.insert lhs ?metaTvScope
            ?ct = ct

        check rhs

        metaDetails <- readIORef metaTvRef
        case metaDetails of
          Flexi -> do
            debug'
              "solve Ct - Flexi"
              [ ("ct", prettyDetailed ct)
              ]
            writeIORef metaTvRef (Indirect rhs)
            pure []
          Indirect ty -> do
            debug'
              "solve Ct - Indirect"
              [ ("ct", prettyDetailed ct)
              ]

            constraintsNew <- unify ct ty rhs

            pure constraintsNew

instance Solve Cts where
  type SolveTo Cts = [Ct]
  solve cts = concat <$> forM (reverse cts.bag) solve

instance Solve Impls where
  type SolveTo Impls = [Implication]
  solve impls = do
    impls' <- forM (reverse impls.bag) $ \impl -> do
      constraintsNew' <- fold <$> solve impl.ic_wanted
      case toListWc constraintsNew' of
        [] -> pure []
        wcs -> pure [impl{ic_wanted = fold wcs}]
    pure $ concat impls'

instance Solve WantedConstraints where
  -- We don't want 'WantedConstraints' with fields containing empty lists.
  -- Therefore, we return a possibly empty list of folded 'WantedConstraints'.
  type SolveTo WantedConstraints = [WantedConstraints]
  solve wcs = do
    wc_simple <- Bag <$> solve wcs.wc_simple
    wc_impl <- Bag <$> solve wcs.wc_impl
    let wcs' = WantedCts{wc_simple, wc_impl}
    pure $ toListWc wcs'

unify :: Ct -> Tau -> Tau -> IO [Ct]
-- Invariant:
-- The first type is "expected",
-- the second one is "actual"
unify ct tv'@(Type'Var tv) ty
  | badType tv' =
      dieSolver SolverError'CannotUnifyBoundVar{tv, ty, ct}
unify ct ty tv'@(Type'Var tv)
  | badType tv' =
      dieSolver SolverError'CannotUnifyBoundVar{tv, ty, ct}
unify
  _ct
  (Type'Var TcTyVar{varDetails = MetaTv{metaTvRef = tv1}})
  (Type'Var TcTyVar{varDetails = MetaTv{metaTvRef = tv2}})
    | tv1 == tv2 = pure []
-- TODO Pass info about swapping?
unify ct (Type'Var tv@TcTyVar{varDetails = MetaTv{}}) ty =
  pure $ unifyVar ct tv ty
unify ct ty (Type'Var tv@TcTyVar{varDetails = MetaTv{}}) =
  pure $ unifyVar ct tv ty
-- TODO is equality defined correctly?
unify _ct (Type'Var tv1@TcTyVar{}) (Type'Var tv2@TcTyVar{})
  | tv1 == tv2 = pure []
unify ct (Type'Fun arg1 res1) (Type'Fun arg2 res2) = do
  -- TODO should we inspect thing?
  cts1 <- unify ct arg1 arg2
  cts2 <- unify ct res1 res2
  pure $ cts1 <> cts2
unify _ct (Type'Concrete tc1) (Type'Concrete tc2)
  | tc1 == tc2 = pure []
unify ct ty1 ty2 =
  dieSolver SolverError'CannotUnify{ty1, ty2, ct}

unifyVar :: Ct -> TcTyVar -> Tau -> [Ct]
unifyVar ct tv@TcTyVar{varDetails = MetaTv{}} ty = do
  pure $
    ct
      { ct_eq_can =
          ct.ct_eq_can
            { eq_lhs = tv
            , eq_rhs = ty
            }
      }
unifyVar _ct _tv _ty = []

-- | Perform occurs and level check.
--
-- Similar to @checkTypeEq@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Solver/Monad.hs#L2328
--
-- Eager unifier in GHC does these checks if the left-hand side
-- of the constraint is a type variable.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L3125
--
-- See Note [EqCt occurs check] in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L263
--
-- See Note [Use checkTyEqRhs in mightEqualLater].
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L4362
class Check a where
  check :: (CtxLhsMetaTv, CtxMetaTvScope, CtxCt) => a -> SolveM ()

-- | Check that a variable somewhere on the rhs
-- of the constraint doesn't have a strictly deeper level
-- than the lhs variable from the constraint.
--
-- Similar to @tyVarLevelCheck@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L3905
levelCheck :: (CtxLhsMetaTv, CtxCt) => TcTyVar -> IO ()
levelCheck rhs = do
  let lhs = ?lhsMetaTv
      lhsLevel = ?lhsMetaTv.varDetails.tcLevel
      rhsLevel = rhs.varDetails.tcLevel

  -- Similar to this check in GHC.
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L2972
  when (rhsLevel > lhsLevel) do
    dieSolver
      SolverError'SkolemEscape
        { tv1 = lhs
        , tv2 = rhs
        , tvTcLevel = lhsLevel
        , tyTcLevel = rhsLevel
        , ct = ?ct
        }

-- | Check whether this metavar is not one of the metavars
-- in whose 'Indirect' types this metavar appeared transitively.
--
-- Similar to @simpleOccursCheck@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L3892
occursCheck :: (CtxLhsMetaTv, CtxCt, CtxMetaTvScope) => TcTyVar -> IO ()
occursCheck var =
  when (Set.member var ?metaTvScope) $
    dieSolver SolverError'OccursCheck{tv = ?lhsMetaTv, ct = ?ct}

-- Similar to @simpleUnifyCheck@ in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L2932
instance Check TcTyVar where
  check var = do
    -- If this is a metavariable, it cannot unify with any metavariable
    -- with a deeper level. Hence, even if the variable has an Indirect type,
    -- that type won't contain any variable with a deeper level.
    --
    -- Therefore, we can check just the level of this metavariable.
    --
    -- See:
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L873
    levelCheck var

    case var.varDetails of
      MetaTv{metaTvRef} -> do
        occursCheck var

        metaDetails <- readIORef metaTvRef
        case metaDetails of
          Flexi -> pure ()
          Indirect ty -> do
            let ?metaTvScope = Set.insert var ?metaTvScope
            check ty
      _ -> pure ()

instance Check TcType where
  check = \case
    Type'Var var -> check var
    Type'ForAll vars body -> do
      forM_ vars check
      check body
    Type'Fun arg res -> do
      check arg
      check res
    Type'Concrete _ -> pure ()

instance Check Ct where
  check ct = do
    let lhs = ct.ct_eq_can.eq_lhs
        rhs = ct.ct_eq_can.eq_rhs

    let ?lhsMetaTv = lhs
        ?metaTvScope = mempty
        ?ct = ct

    check rhs

instance Check Cts where
  check cts = forM_ cts.bag check

instance Check Impls where
  check impls =
    forM_ (reverse impls.bag) (\impl -> check impl.ic_wanted)

instance Check WantedConstraints where
  check wcs = do
    check wcs.wc_simple
    check wcs.wc_impl

toListWc :: WantedConstraints -> [WantedConstraints]
toListWc = \case
  WantedCts{wc_simple = Bag [], wc_impl = Bag []} -> []
  x -> [x]

fromListWc :: [WantedConstraints] -> WantedConstraints
fromListWc = fold

solveIteratively :: Int -> WantedConstraints -> SolveM WantedConstraints
solveIteratively iterations constraints = do
  let ?metaTvScope = Set.empty
  constraints' <- go iterations constraints

  -- TODO run check

  -- All unification variables are in the initial constraints.
  -- We need to check that these variables don't occur in their types.

  -- check constraints

  pure constraints'
 where
  go :: (CtxMetaTvScope) => Int -> WantedConstraints -> SolveM WantedConstraints
  go _ constraintsCurrent
    | [] <- toListWc constraintsCurrent =
        pure constraintsCurrent
  go 0 constraintsCurrent = pure constraintsCurrent
  go n constraintsCurrent = do
    debug'
      ("go" <+> pretty' n)
      [ ("constraintsCurrent", prettyDetailed constraintsCurrent)
      ]
    constraintsNew <- fold <$> solve constraintsCurrent
    debug'
      ("go" <+> pretty' n)
      [ ("constraintsNew", prettyDetailed constraintsNew)
      ]
    go (n - 1) constraintsNew

-- ==============================================
-- Renamer errors
-- ==============================================

data SolverError
  = SolverError'CannotUnifySkolemVar {ct :: Ct}
  | SolverError'CannotUnifyBoundVar {tv :: TcTyVar, ty :: TcType, ct :: Ct}
  | SolverError'CannotUnify {ty1 :: TcType, ty2 :: TcType, ct :: Ct}
  | SolverError'SkolemEscape {tv1 :: TcTyVar, tv2 :: TcTyVar, tvTcLevel :: TcLevel, tyTcLevel :: TcLevel, ct :: Ct}
  | SolverError'OccursCheck {tv :: TcTyVar, ct :: Ct}

data SolverErrorWithCallStack where
  SolverErrorWithCallStack :: (HasCallStack) => SolverError -> SolverErrorWithCallStack

-- | Fail unconditionally
dieSolver :: (HasCallStack) => SolverError -> IO a
dieSolver err = throw (SolverErrorWithCallStack err)

instance Pretty' SolverError where
  pretty' = \case
    SolverError'CannotUnifySkolemVar{ct} ->
      vsep' $
        [ "Cannot unify the skolem variable:"
        , prettyIndent ct.ct_eq_can.eq_lhs
        , "with the type:"
        , prettyIndent ct.ct_eq_can.eq_rhs
        ]
          <> inTheConstraint ct
    SolverError'CannotUnifyBoundVar{tv, ty, ct} ->
      vsep' $
        [ "Cannot unify bound variable:"
        , prettyIndent tv
        , "with the type:"
        , prettyIndent ty
        ]
          <> inTheConstraint ct
    SolverError'CannotUnify{ty1, ty2, ct} ->
      vsep' $
        [ "Cannot unify the type:"
        , prettyIndent ty1
        , "with the type:"
        , prettyIndent ty2
        ]
          <> inTheConstraint ct
    SolverError'SkolemEscape{tv1, tv2, tvTcLevel, tyTcLevel, ct} ->
      vsep' $
        [ "Skolem escape!"
        , "The variable:"
        , prettyIndent tv1
        , "has the TcLevel:"
        , prettyIndent tvTcLevel
        , "but the type variable:"
        , prettyIndent tv2
        , "has a larger TcLevel:"
        , prettyIndent tyTcLevel
        ]
          <> inTheConstraint ct
    SolverError'OccursCheck{ct} ->
      vsep' $
        [ "Occurs check failed!"
        , "Type variable:"
        , prettyIndent ct.ct_eq_can.eq_lhs
        , "occurs in the type:"
        , prettyIndent ct.ct_eq_can.eq_rhs
        ]
          <> inTheConstraint ct
   where
    inTheConstraint ct =
      [ "in the constraint:"
      , prettyIndent ct
      ]

instance Pretty' SolverErrorWithCallStack where
  pretty' (SolverErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]

instance Exception SolverError

instance Exception SolverErrorWithCallStack
