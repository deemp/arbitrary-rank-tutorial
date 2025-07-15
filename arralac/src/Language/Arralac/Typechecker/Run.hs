{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Arralac.Typechecker.Run where

import Data.IORef (newIORef, readIORef)
import GHC.Stack (HasCallStack)
import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Debug (debug')
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Prelude.Unique.Supply
import Language.Arralac.Solver.Run
import Language.Arralac.Solver.Types
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Typechecker.Constraints (emptyWantedConstraints)
import Language.Arralac.Typechecker.TcTerm (inferRho)
import Language.Arralac.Typechecker.TcTyVarEnv (emptyTcTyVarEnv)
import Language.Arralac.Typechecker.Types
import UnliftIO (finally)

-- TODO use bluefin to expose exceptions

-- TODO report many independent errors, not fail on the first error
runTypechecker ::
  ( HasCallStack
  , CtxSolverIterations
  , CtxDebug
  , CtxPrettyVerbosity
  , CtxUniqueSupply
  ) =>
  SynTerm CompRn -> IO (SynTerm CompTc)
runTypechecker term = do
  constraints <- newIORef emptyWantedConstraints
  let ?constraints = constraints
      ?tcLevel = TcLevel 0
      ?tcTyVarEnv = emptyTcTyVarEnv
      ?tcErrorPropagated = Nothing
  do
    -- TODO run inferSigma
    (tcTerm, _) <- inferRho term
    constraintsNew <- readIORef constraints
    _ <- runSolver ?solverIterations constraintsNew
    pure tcTerm
    `finally` do
      constraints' <- readIORef ?constraints
      debug'
        "typecheck"
        [ ("constraints", pretty' constraints')
        ]