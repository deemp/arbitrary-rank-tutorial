{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Solver.Error where

import Control.Exception (Exception, throw)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Type.Local.TyVar.Tc
import Language.Arralac.Type.Local.Type
import Language.Arralac.Typechecker.Constraints
import Language.Arralac.Typechecker.Types

data SolverError
  = SolverError'CannotUnifySkolemVar {ct :: Ct}
  | SolverError'CannotUnifyBoundVar {tv :: TcTyVar, ty :: TcType, ct :: Ct}
  | SolverError'CannotUnify {ty1 :: TcType, ty2 :: TcType, ct :: Ct}
  | SolverError'SkolemEscape {tv1 :: TcTyVar, tv2 :: TcTyVar, tvTcLevel :: TcLevel, tyTcLevel :: TcLevel, ct :: Ct}
  | SolverError'OccursCheck {tv :: TcTyVar, ct :: Ct}

data SolverErrorWithCallStack where
  SolverErrorWithCallStack :: (HasCallStack) => SolverError -> SolverErrorWithCallStack

-- | Fail unconditionally
dieSolver :: (HasCallStack) => SolverError -> IO a
dieSolver err = throw (SolverErrorWithCallStack err)

instance Pretty' SolverError where
  pretty' = \case
    SolverError'CannotUnifySkolemVar{ct} ->
      vsep' $
        [ "Cannot unify the skolem variable:"
        , prettyIndent ct.ct_eq_can.eq_lhs
        , "with the type:"
        , prettyIndent ct.ct_eq_can.eq_rhs
        ]
          <> inTheConstraint ct
    SolverError'CannotUnifyBoundVar{tv, ty, ct} ->
      vsep' $
        [ "Cannot unify bound variable:"
        , prettyIndent tv
        , "with the type:"
        , prettyIndent ty
        ]
          <> inTheConstraint ct
    SolverError'CannotUnify{ty1, ty2, ct} ->
      vsep' $
        [ "Cannot unify the type:"
        , prettyIndent ty1
        , "with the type:"
        , prettyIndent ty2
        ]
          <> inTheConstraint ct
    SolverError'SkolemEscape{tv1, tv2, tvTcLevel, tyTcLevel, ct} ->
      vsep' $
        [ "Skolem escape!"
        , "The variable:"
        , prettyIndent tv1
        , "has the TcLevel:"
        , prettyIndent tvTcLevel
        , "but the type variable:"
        , prettyIndent tv2
        , "has a larger TcLevel:"
        , prettyIndent tyTcLevel
        ]
          <> inTheConstraint ct
    SolverError'OccursCheck{ct} ->
      vsep' $
        [ "Occurs check failed!"
        , "Type variable:"
        , prettyIndent ct.ct_eq_can.eq_lhs
        , "occurs in the type:"
        , prettyIndent ct.ct_eq_can.eq_rhs
        ]
          <> inTheConstraint ct
   where
    inTheConstraint ct =
      [ "in the constraint:"
      , prettyIndent ct
      ]

instance Pretty' SolverErrorWithCallStack where
  pretty' (SolverErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]

instance Exception SolverError

instance Exception SolverErrorWithCallStack
