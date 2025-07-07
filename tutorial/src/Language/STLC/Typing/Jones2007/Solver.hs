{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.STLC.Typing.Jones2007.Solver where

import Control.Monad (forM, forM_, when)
import Data.Foldable (Foldable (..))
import Data.IORef (readIORef, writeIORef)
import GHC.IORef (newIORef)
import Language.STLC.Typing.Jones2007.Bag
import Language.STLC.Typing.Jones2007.BasicTypes
import Language.STLC.Typing.Jones2007.ConstraintTypes
import Language.STLC.Typing.Jones2007.TcMonad (TcError (..), TcM, die, readTcRef, unify, writeTcRef)

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

type SolveM a b = a -> TcM b

class Solve a where
  type Ret a
  solve :: SolveM a (Ret a)
  occursCheck :: SolveM a ()

toListWc :: WantedConstraints -> [WantedConstraints]
toListWc = \case
  WantedCts{wc_simple = Bag [], wc_impl = Bag []} -> []
  x -> [x]

fromListWc :: [WantedConstraints] -> WantedConstraints
fromListWc = fold

instance Solve Ct where
  type Ret Ct = [WantedConstraints]
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
                pure []
              Indirect ty -> do
                writeTcRef ?constraints emptyWantedConstraints
                -- TODO propagate Ct info into each new constraint?
                unify Nothing ty rhs
                -- Have to do this because unify doesn't return constraints
                constraintsNew <- readTcRef ?constraints
                writeTcRef ?constraints emptyWantedConstraints
                pure $ toListWc constraintsNew
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
  type Ret Cts = [WantedConstraints]
  solve cts = concat <$> forM (reverse cts.bag) solve
  occursCheck cts = forM_ cts.bag occursCheck

instance Solve Impls where
  type Ret Impls = [Implication]
  solve impls = do
    impls' <- forM (reverse impls.bag) $ \impl -> do
      constraintsNew' <- fold <$> solve impl.ic_wanted
      case toListWc constraintsNew' of
        [] -> pure []
        wcs -> pure [impl{ic_wanted = fold wcs}]
    pure $ concat impls'
  occursCheck impls =
    forM_ (reverse impls.bag) (\impl -> occursCheck impl.ic_wanted)

instance Solve WantedConstraints where
  type Ret WantedConstraints = [WantedConstraints]
  solve wcs = do
    wc_simple <- fold <$> solve wcs.wc_simple
    wc_impl <- solve wcs.wc_impl
    let wcs' = wc_simple <> WantedCts{wc_simple = emptyBag, wc_impl = Bag wc_impl}
    pure $ toListWc wcs'
  occursCheck wcs = do
    occursCheck wcs.wc_simple
    occursCheck wcs.wc_impl

solveIteratively :: TcM ()
solveIteratively = do
  forM_ ([0 .. 10] :: [Int]) $ \_ -> do
    constraintsCurrent <- readIORef ?constraints

    -- Don't modify any external mutable variables.
    constraintsNew <- newIORef emptyWantedConstraints
    tcErrorPropagated <- newIORef Nothing
    let ?constraints = constraintsNew
        ?tcErrorPropagated = tcErrorPropagated

    wcs <- solve constraintsCurrent

    writeIORef ?constraints (fold wcs)