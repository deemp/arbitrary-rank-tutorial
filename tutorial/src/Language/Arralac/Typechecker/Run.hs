{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Arralac.Typechecker.Run where

import Data.IORef (newIORef, readIORef)
import Data.Text qualified as T
import GHC.Stack
import Language.Arralac.Parser.Parse
import Language.Arralac.Renamer.Run (convertRenameAbs)
import Language.Arralac.Solver.Types (CtxSolverIterations)
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Typechecker.Constraints (CtxWantedConstraints, emptyWantedConstraints)
import Language.Arralac.Typechecker.Error (CtxTcErrorPropagated)
import Language.Arralac.Typechecker.Solver (solveIteratively)
import Language.Arralac.Typechecker.TcTerm (inferRho)
import Language.Arralac.Typechecker.TcTyVarEnv (CtxTcTyVarEnv, emptyTcTyVarEnv)
import Language.Arralac.Typechecker.Types (TcLevel (..))
import Language.Arralac.Utils.Debug (debug')
import Language.Arralac.Utils.Pass
import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types
import Language.Arralac.Utils.Unique.Supply (CtxUniqueSupply)
import Language.Arralac.Zonker.Zn.Zonk (Zonk (..))
import UnliftIO.Exception (finally)

-- TODO use bluefin to expose exceptions

-- TODO report many independent errors, not fail on the first error
typecheck ::
  ( CtxSolverIterations
  , CtxDebug
  , CtxPrettyVerbosity
  , CtxUniqueSupply
  , CtxTcTyVarEnv
  , CtxTcErrorPropagated
  , CtxWantedConstraints
  ) =>
  SynTerm CompRn -> IO (SynTerm CompZn)
typecheck term =
  do
    constraints <- newIORef emptyWantedConstraints
    let ?constraints = constraints
        ?tcLevel = TcLevel 0
    -- TODO run inferSigma
    (tcTerm, _) <- inferRho term
    constraintsNew <- readIORef ?constraints
    _ <- solveIteratively ?solverIterations constraintsNew

    zonk tcTerm
    `finally` do
      constraints <- readIORef ?constraints
      debug'
        "typecheck"
        [ ("constraints", pretty' constraints)
        ]

runTypechecker :: (HasCallStack, CtxDebug, CtxUniqueSupply, CtxPrettyVerbosity, CtxSolverIterations) => SynTerm CompRn -> IO (SynTerm CompZn)
runTypechecker program = do
  constraints <- newIORef emptyWantedConstraints
  tcError <- newIORef Nothing
  let
    ?tcLevel = TcLevel 0
    ?tcTyVarEnv = emptyTcTyVarEnv
    ?constraints = constraints
    ?tcErrorPropagated = tcError
  typecheck program

-- TODO use filepath from implicit params
runTypechecker' :: (HasCallStack, CtxDebug, CtxPrettyVerbosity, CtxSolverIterations) => FastString -> T.Text -> IO (SynTerm CompZn)
runTypechecker' filePath content = do
  uniqueSupply <- newIORef 0
  let ?uniqueSupply = uniqueSupply
      ?currentFilePath = filePath
  program <- do
    let
    parseText content >>= convertRenameAbs
  runTypechecker program
