module Language.Arralac.Solver.Run where

import Data.Foldable
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Language.Arralac.Prelude.Debug
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Solver.Solve
import Language.Arralac.Solver.Types
import Language.Arralac.Typechecker.Constraints
import Prettyprinter

runSolver ::
  (HasCallStack, CtxDebug, CtxPrettyVerbosity) =>
  Int -> WantedConstraints -> IO WantedConstraints
runSolver iterations constraints = do
  let ?metaTvScope = Set.empty
  runSolver' iterations constraints

runSolver' :: (HasCallStack, CtxMetaTvScope, CtxDebug, CtxPrettyVerbosity) => Int -> WantedConstraints -> IO WantedConstraints
runSolver' _ constraintsCurrent
  | [] <- toListWc constraintsCurrent =
      pure constraintsCurrent
runSolver' 0 constraintsCurrent = pure constraintsCurrent
runSolver' n constraintsCurrent = do
  debug'
    ("runSolver'" <+> pretty' n)
    [ ("constraintsCurrent", prettyDetailed constraintsCurrent)
    ]
  constraintsNew <- fold <$> solve constraintsCurrent
  debug'
    ("runSolver'" <+> pretty' n)
    [ ("constraintsNew", prettyDetailed constraintsNew)
    ]
  runSolver' (n - 1) constraintsNew
