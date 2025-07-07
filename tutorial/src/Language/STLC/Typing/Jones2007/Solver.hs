{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

module Language.STLC.Typing.Jones2007.Solver where

import Control.Monad (forM_, when)
import Language.STLC.Typing.Jones2007.Bag
import Language.STLC.Typing.Jones2007.BasicTypes
import Language.STLC.Typing.Jones2007.ConstraintTypes
import Language.STLC.Typing.Jones2007.TcMonad (ITcErrorPropagated, TcError (..), die, readTcRef, writeTcRef)

class MaxTcLevel a where
  getMaxTcLevel :: a -> TcLevel

instance MaxTcLevel TcTyVar where
  getMaxTcLevel var = var.varDetails.tcLevel

instance MaxTcLevel TcType where
  getMaxTcLevel = \case
    Type'Var var -> var.varDetails.tcLevel
    -- 'maximum' is safe to use because
    -- the structure isn't empty
    Type'ForAll vars body -> maximum ((getMaxTcLevel <$> vars) <> [getMaxTcLevel body])
    Type'Fun arg res -> maximum ([getMaxTcLevel arg, getMaxTcLevel res])
    Type'Concrete _ -> 0

type SolveM a = (ITcErrorPropagated) => a -> IO ()

class Solve a where
  recurse :: ((Solve b) => SolveM b) -> SolveM a
  solve :: SolveM a
  solve = recurse solve
  occursCheck :: SolveM a
  occursCheck = recurse occursCheck

instance Solve Ct where
  recurse _ _ = pure ()
  solve ct = do
    let lhs = ct.ct_eq_can.eq_lhs
        rhs = ct.ct_eq_can.eq_rhs
    case lhs.varDetails of
      SkolemTv{} -> die TcError'CannotUnifySkolemVar{ct}
      BoundTv{} -> die TcError'CannotUnifyBoundVar{ct}
      MetaTv{metaTvRef} -> do
        let lhsLevel = getMaxTcLevel lhs
            rhsLevel = getMaxTcLevel rhs
        if rhsLevel <= lhsLevel
          then do
            metaDetails <- readTcRef metaTvRef
            case metaDetails of
              Flexi -> do
                writeTcRef metaTvRef (Indirect rhs)
              -- TODO unify
              Indirect _ty -> pure ()
          else die TcError'SkolemEscape{ct}
  occursCheck ct = do
    let lhs = ct.ct_eq_can.eq_lhs
        rhs = ct.ct_eq_can.eq_rhs
        -- TODO recurse into Indirect type in Type'Var
        -- Can this cause infinite recursion?
        occursIn :: TcTyVar -> TcType -> Bool
        occursIn var = \case
          Type'Var var' -> var == var'
          Type'ForAll vars body -> var `elem` vars || var `occursIn` body
          Type'Fun arg res -> var `occursIn` arg || var `occursIn` res
          Type'Concrete _ -> False
    when (lhs `occursIn` rhs) $ die TcError'OccursCheck{ct}

instance Solve Cts where
  recurse act cts = forM_ (reverse cts.bag) (recurse act)

instance Solve Impls where
  recurse act impls = forM_ (reverse impls.bag) (recurse act . (.ic_wanted))

instance Solve WantedConstraints where
  recurse act wcs = do
    recurse act wcs.wc_simple
    recurse act wcs.wc_impl
