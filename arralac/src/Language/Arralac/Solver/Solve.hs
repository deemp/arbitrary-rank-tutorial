{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Solver.Solve where

import Control.Monad (forM)
import Data.Foldable (Foldable (..))
import Data.IORef (readIORef, writeIORef)
import Data.Set qualified as Set
import Language.Arralac.Prelude.Bag
import Language.Arralac.Prelude.Debug (debug')
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Solver.Check
import Language.Arralac.Solver.Error
import Language.Arralac.Solver.Types
import Language.Arralac.Solver.Unify qualified as Solver
import Language.Arralac.Type.Local.TyVar.Tc
import Language.Arralac.Typechecker.Constraints

type SolveM a = (CtxDebug, CtxPrettyVerbosity) => IO a

-- | Solve constraints.
--
-- Limitations:
--
-- No promotion. See Note [Promotion and level-checking] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L3588
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
        do
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

              constraintsNew <- Solver.unify ct ty rhs

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

toListWc :: WantedConstraints -> [WantedConstraints]
toListWc = \case
  WantedCts{wc_simple = Bag [], wc_impl = Bag []} -> []
  x -> [x]

fromListWc :: [WantedConstraints] -> WantedConstraints
fromListWc = fold
