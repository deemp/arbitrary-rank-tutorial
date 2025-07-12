module Language.Arralac.Typecheck.Run where

import Data.IORef (newIORef, readIORef)
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Stack
import Language.Arralac.Parser
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Typecheck.Constraints (emptyWantedConstraints)
import Language.Arralac.Typecheck.Pass
import Language.Arralac.Typecheck.Renamer (ConvertRename (convertRename))
import Language.Arralac.Typecheck.Solver (solveIteratively)
import Language.Arralac.Typecheck.TcMonad
import Language.Arralac.Typecheck.TcTerm (inferRho)
import Language.Arralac.Typecheck.Zonker (Zonk (..))
import Language.Arralac.Utils.Pretty
import Language.Arralac.Utils.Types
import Language.Arralac.Utils.Unique.Supply (IUniqueSupply)
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

runTypechecker :: (HasCallStack, IDebug, IUniqueSupply, IPrettyVerbosity, ISolverIterations) => SynTerm CompRn -> IO (SynTerm CompZn)
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
runTypechecker' :: (HasCallStack, IDebug, IPrettyVerbosity, ISolverIterations) => FastString -> T.Text -> IO (SynTerm CompZn)
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
    parseText content >>= convertRename
  runTypechecker program
