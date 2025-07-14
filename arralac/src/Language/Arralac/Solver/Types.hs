module Language.Arralac.Solver.Types where

import Data.Set
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Typechecker.Constraints

type CtxSolverIterations = (?solverIterations :: Int)

type CtxLhsMetaTv = (?lhsMetaTv :: TcTyVar)

type CtxMetaTvScope = (?metaTvScope :: Set TcTyVar)

type CtxCt = (?ct :: Ct)
