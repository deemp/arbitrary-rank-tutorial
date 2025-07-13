module Language.Arralac.Solver.Check where

import Control.Monad
import Data.IORef (readIORef)
import Data.Set qualified as Set
import Language.Arralac.Prelude.Bag
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Solver.Error
import Language.Arralac.Solver.Types
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.Type
import Language.Arralac.Typechecker.Constraints

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
  check :: (CtxLhsMetaTv, CtxMetaTvScope, CtxCt, CtxDebug, CtxPrettyVerbosity) => a -> IO ()

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
  check cts =
    forM_ cts.bag check

instance Check Impls where
  check impls =
    forM_ (reverse impls.bag) (\impl -> check impl.ic_wanted)

instance Check WantedConstraints where
  check wcs = do
    check wcs.wc_simple
    check wcs.wc_impl
