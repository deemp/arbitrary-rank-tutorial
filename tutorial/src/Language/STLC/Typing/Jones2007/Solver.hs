{-# LANGUAGE OverloadedLabels #-}

module Language.STLC.Typing.Jones2007.Solver where

import Control.Exception (Exception, throw)
import Control.Monad (forM, forM_, when)
import Data.Foldable (Foldable (..))
import Data.IORef (readIORef, writeIORef)
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Language.STLC.Typing.Jones2007.Bag
import Language.STLC.Typing.Jones2007.BasicTypes
import Language.STLC.Typing.Jones2007.Constraints
import Language.STLC.Typing.Jones2007.TcMonad (IConstraints, ITcErrorPropagated, TcError (..), TcM, badType, debug, die, readTcRef, writeTcRef)
import Prettyprinter (indent)

data SolverError
  = SolverError'CannotUnifySkolemVar {ct :: Ct}
  | SolverError'CannotUnifyBoundVar {tv :: TcTyVar, ty :: TcType, ct :: Ct}
  | SolverError'CannotUnify {ty1 :: TcType, ty2 :: TcType, ct :: Ct}
  | SolverError'SkolemEscape {tv :: TcTyVar, ty :: TcType, tvTcLevel :: TcLevel, tyTcLevel :: TcLevel, ct :: Ct}

data SolverErrorWithCallStack where
  SolverErrorWithCallStack :: (HasCallStack) => SolverError -> SolverErrorWithCallStack

-- | Fail unconditionally
dieSolver :: (HasCallStack) => SolverError -> IO a
dieSolver err = throw (SolverErrorWithCallStack err)

type SolveM a b = a -> TcM b

toListWc :: WantedConstraints -> [WantedConstraints]
toListWc = \case
  WantedCts{wc_simple = Bag [], wc_impl = Bag []} -> []
  x -> [x]

fromListWc :: [WantedConstraints] -> WantedConstraints
fromListWc = fold

unify :: (ITcErrorPropagated, IConstraints) => Ct -> Tau -> Tau -> IO [Ct]
-- Invariant:
-- The first type is "expected",
-- the second one is "actual"
unify ct tv'@(Type'Var tv) ty
  | badType tv' =
      dieSolver SolverError'CannotUnifyBoundVar{tv, ty, ct}
unify ct ty tv'@(Type'Var tv)
  | badType tv' =
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

class MaxTcLevel a where
  getMaxTcLevel :: a -> TcLevel

instance MaxTcLevel TcTyVar where
  getMaxTcLevel var = var.varDetails.tcLevel

instance MaxTcLevel TcType where
  getMaxTcLevel = \case
    Type'Var var -> var.varDetails.tcLevel
    -- 'maximum' is safe to use because the structure isn't empty
    Type'ForAll vars body -> maximum ((getMaxTcLevel <$> vars) <> [getMaxTcLevel body])
    Type'Fun arg res -> maximum ([getMaxTcLevel arg, getMaxTcLevel res])
    Type'Concrete _ -> 0

class Solve a where
  type Ret a
  solve :: SolveM a (Ret a)
  occursCheck :: SolveM a ()

instance Solve Ct where
  type Ret Ct = [Ct]
  solve ct = do
    let lhs = ct.ct_eq_can.eq_lhs
        rhs = ct.ct_eq_can.eq_rhs
    case lhs.varDetails of
      -- TODO why can we get not a metavar on LHS?
      SkolemTv{} -> dieSolver SolverError'CannotUnifySkolemVar{ct}
      BoundTv{} -> dieSolver SolverError'CannotUnifyBoundVar{tv = lhs, ty = rhs, ct}
      MetaTv{metaTvRef} -> do
        let lhsLevel = getMaxTcLevel lhs
            rhsLevel = getMaxTcLevel rhs
        if
          | lhsLevel >= rhsLevel -> do
              metaDetails <- readIORef metaTvRef
              case metaDetails of
                Flexi -> do
                  debug'
                    "solve Ct - Flexi"
                    [ ("ct", prettyVerbose ct)
                    ]
                  writeIORef metaTvRef (Indirect rhs)
                  pure []
                Indirect ty -> do
                  debug'
                    "solve Ct - Indirect"
                    [ ("ct", prettyVerbose ct)
                    ]
                  constraintsNew <- unify ct ty rhs
                  pure constraintsNew
          | otherwise ->
              dieSolver
                SolverError'SkolemEscape
                  { tv = lhs
                  , ty = rhs
                  , tvTcLevel = lhsLevel
                  , tyTcLevel = rhsLevel
                  , ct
                  }

  occursCheck ct = do
    let lhs = ct.ct_eq_can.eq_lhs
        rhs = ct.ct_eq_can.eq_rhs
        -- TODO Should we recurse into Indirect type in Type'Var?
        -- This can cause infinite recursion.
        occursIn :: TcTyVar -> TcType -> Bool
        occursIn var = \case
          Type'Var var' -> var == var'
          Type'ForAll vars body -> var `elem` vars || var `occursIn` body
          Type'Fun arg res -> var `occursIn` arg || var `occursIn` res
          Type'Concrete _ -> False
    when (lhs `occursIn` rhs) $ die TcError'OccursCheck{ct}

instance Solve Cts where
  type Ret Cts = [Ct]
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
  -- We don't want 'WantedConstraints' with fields containing empty lists.
  -- Therefore, we return a possibly empty list of folded 'WantedConstraints'.
  type Ret WantedConstraints = [WantedConstraints]
  solve wcs = do
    wc_simple <- Bag <$> solve wcs.wc_simple
    wc_impl <- Bag <$> solve wcs.wc_impl
    let wcs' = WantedCts{wc_simple, wc_impl}
    pure $ toListWc wcs'
  occursCheck wcs = do
    occursCheck wcs.wc_simple
    occursCheck wcs.wc_impl

instance Pretty' SolverError where
  pretty' = \case
    SolverError'CannotUnifySkolemVar{ct} ->
      vsep'
        [ "Cannot unify the skolem variable:"
        , prettyIndent ct.ct_eq_can.eq_lhs
        , "with the type:"
        , prettyIndent ct.ct_eq_can.eq_rhs
        , "in the constraint:"
        , prettyIndent ct
        ]
    SolverError'CannotUnifyBoundVar{tv, ty, ct} ->
      vsep'
        [ "Cannot unify bound variable:"
        , prettyIndent tv
        , "with the type:"
        , prettyIndent ty
        , "in the constraint:"
        , prettyIndent ct
        ]
    SolverError'CannotUnify{ty1, ty2, ct} ->
      vsep'
        [ "Cannot unify the type:"
        , prettyIndent ty1
        , "with the type:"
        , prettyIndent ty2
        , "in the constraint:"
        , prettyIndent ct
        ]
    SolverError'SkolemEscape{tv, ty, tvTcLevel, tyTcLevel, ct} ->
      vsep'
        [ "Skolem escape!"
        , "The variable:"
        , prettyIndent tv
        , "has the TcLevel:"
        , prettyIndent tvTcLevel
        , "but the type:"
        , prettyIndent ty
        , "has a larger TcLevel:"
        , prettyIndent tyTcLevel
        , "in the constraint:"
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