module Language.Arralac.Solver.Unify where

import Language.Arralac.Solver.Error
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.Type
import Language.Arralac.Typechecker.Constraints
import Language.Arralac.Typechecker.TcTyVar

unify :: Ct -> Tau -> Tau -> IO [Ct]
-- Invariant:
-- The first type is "expected",
-- the second one is "actual".
unify ct tv'@(Type'Var tv) ty
  | isBoundTvTypeVar tv' =
      dieSolver SolverError'CannotUnifyBoundVar{tv, ty, ct}
unify ct ty tv'@(Type'Var tv)
  | isBoundTvTypeVar tv' =
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