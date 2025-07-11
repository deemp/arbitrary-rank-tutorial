module Language.Arralac.Typecheck.Run where

import Data.IORef (newIORef, readIORef)
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import Language.Arralac.Typecheck.Jones2007.BasicTypes
import Language.Arralac.Typecheck.Jones2007.BasicTypes qualified as BT
import Language.Arralac.Typecheck.Jones2007.Constraints (emptyWantedConstraints)
import Language.Arralac.Typecheck.Jones2007.Solver (solveIteratively)
import Language.Arralac.Typecheck.Jones2007.TcMonad
import Language.Arralac.Typecheck.Jones2007.TcTerm (inferRho)
import Language.Arralac.Typecheck.Renamer (parseInputText)
import Language.Arralac.Typecheck.Zonker (Zonk (..))
import UnliftIO.Exception (finally)

-- ==============================================
--      The top-level wrapper
-- ==============================================

-- TODO Too much boilerplate...

-- TODO use bluefin to expose exceptions

-- TODO report many independent errors, not fail on the first error
typecheck :: SynTerm CompRn -> TcM (SynTerm CompZn)
typecheck term =
  do
    constraints <- newIORef emptyWantedConstraints
    let ?constraints = constraints
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

runTypechecker :: (IDebug, IUniqueSupply, IPrettyVerbosity, ISolverIterations) => SynTerm BT.CompRn -> IO (SynTerm BT.CompZn)
runTypechecker program = do
  constraints <- newIORef emptyWantedConstraints
  tcError <- newIORef Nothing
  let
    ?tcLevel = BT.TcLevel 0
    ?tcTyVarEnv = emptyTcTyVarEnv
    ?constraints = constraints
    ?tcErrorPropagated = tcError
  typecheck program

-- TODO use filepath from implicit params
runTypechecker' :: (HasCallStack, IDebug, IPrettyVerbosity, ISolverIterations) => BT.FastString -> T.Text -> IO (SynTerm CompZn)
runTypechecker' filePath content = do
  uniqueSupply <- newIORef 0
  let ?uniqueSupply = uniqueSupply
      ?currentFilePath = filePath
  program <- do
    let
      ?termVarScope = Map.empty
      ?tyVarScope = Map.empty
      -- TODO put existing types here
      ?tyConcreteScope = Map.empty
    parseInputText content
  runTypechecker program
