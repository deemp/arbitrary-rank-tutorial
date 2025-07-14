module Language.Arralac.Solver.Run where

import Data.Foldable
import Data.Set qualified as Set
import Language.Arralac.Prelude.Debug
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Solver.Solve
import Language.Arralac.Solver.Types
import Language.Arralac.Typechecker.Constraints
import Prettyprinter

runSolver ::
  (CtxDebug, CtxPrettyVerbosity) =>
  Int -> WantedConstraints -> IO WantedConstraints
runSolver iterations constraints = do
  let ?metaTvScope = Set.empty
  do
    constraints' <- go iterations constraints
    pure constraints'
 where
  go :: (CtxMetaTvScope, CtxDebug, CtxPrettyVerbosity) => Int -> WantedConstraints -> IO WantedConstraints
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
