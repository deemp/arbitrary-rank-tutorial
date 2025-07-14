module Language.Arralac.Typechecker.Constraints where

import Data.IORef (IORef)
import GHC.Generics (Generic)
import Language.Arralac.Prelude.Bag (Bag (..), emptyBag)
import Language.Arralac.Prelude.Pass (CompRn)
import Language.Arralac.Prelude.Pretty (Pretty' (..), genericPretty)
import Language.Arralac.Syntax.Local.Name (RealSrcSpan)
import Language.Arralac.Syntax.Local.SynTerm.Rn ()
import Language.Arralac.Syntax.Local.SynTerm.Tc ()
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Syntax.Local.Type (Tau, TcType)
import Language.Arralac.Syntax.TTG.SynTerm (SynTerm)
import Language.Arralac.Syntax.TTG.Type (Type (..))
import Language.Arralac.Typechecker.Types (TcLevel)

type CtxWantedConstraints = (?constraints :: IORef WantedConstraints)

-- | Constraints that we want to solve.
--
-- Similar to @WantedConstraints@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1089
data WantedConstraints = WantedCts
  { wc_simple :: Cts
  , wc_impl :: Impls
  }
  deriving stock (Generic)

-- | A 'Bag' of constraints ('Ct').
--
-- Similar to @Cts@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L164
type Cts = Bag Ct

-- | A 'Bag' of 'Implication's.
--
-- Similar to 'Cts'.
type Impls = Bag Implication

-- | A constraint.
--
-- Similar to @Ct@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L198
--
-- Out of simple constraints, we only have equality constraints.
--
-- See @WantedConstraints@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1090
data Ct = CEqCan
  { ct_eq_can :: EqCt
  -- ^ A canonical equality constraint.
  --
  -- See Note [Canonicalization] in GHC.
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Solver/Solve.hs#L1033
  --
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Monad.hs#L2057
  }
  deriving stock (Generic)

-- TODO ^ store how a constraint appeared.
-- For equality, store previous constraints.

-- | Equality constraint.
--
-- Similar to @EqCt@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L299
data EqCt = EqCt
  { eq_ev :: CtEvidence
  , eq_lhs :: TcTyVar
  , eq_rhs :: TcType
  }
  deriving stock (Generic)

-- | Constraint evidence.
--
-- Similar to @CtEvidence@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L2166
--
-- We don't have given constraints
-- because we don't have constraints on types
-- (the <constraints> part in <constraints> => <type>)
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/constraint_kind.html
data CtEvidence = CtWanted
  { ct_ev_wanted :: WantedCtEvidence
  }
  deriving stock (Generic)

-- | Evidence for a Wanted constraint.
--
-- Similar to @WantedCtEvidence@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L2178
--
-- We want to keep things simple, so we don't rewrite wanteds.
--
-- See Note [Wanteds rewrite Wanteds] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L2415
data WantedCtEvidence
  = WantedCt
  { ctev_loc :: CtLoc
  }
  deriving stock (Generic)

-- | Constraint location information.
--
-- We don't need:
-- `ctl_t_or_k` because we don't have kinds.
-- `ctl_depth` because we don't have equalities involving type functions.
--
-- Similar to @CtLoc@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L123
data CtLoc = CtLoc
  { ctl_origin :: CtOrigin
  , -- TODO remove Maybe
    ctl_env :: Maybe CtLocEnv
  -- ^ Everything we need to know about
  -- the context this Ct arose in.
  }
  deriving stock (Generic)

-- | Constraint origin.
--
-- Similar to @CtOrigin@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L491
data CtOrigin
  = TypeEqOrigin
  { uo_actual :: TcType
  , uo_expected :: TcType
  , uo_thing :: Maybe TypedThing
  -- ^ The thing that has type "actual"
  }
  deriving stock (Generic)

-- | Local typechecker environment for a constraint.
--
-- Used to restore the environment of a constraint
-- when reporting errors.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L227
data CtLocEnv = CtLocEnv
  { ctl_loc :: !RealSrcSpan
  -- TODO add more data?
  }
  deriving stock (Generic)

-- | Implication constraint.
--
-- Don't need origin because we don't mention
-- the implication in errors.
-- Rather, we mention type variables
-- introduced in the implication.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1408
data Implication = Implic
  { ic_tclvl :: TcLevel
  -- ^ TcLevel of unification variables
  -- allocated /inside/ this implication
  , ic_skols :: [TcTyVar]
  -- ^ Introduced skolems; always skolem TcTyVars
  -- Their level numbers should be precisely ic_tclvl
  -- Their SkolemInfo should be precisely ic_info (almost)
  --       See Note [Implication invariants]
  -- TODO remove Maybe
  , ic_env :: Maybe CtLocEnv
  -- ^ Records the context at the time of creation.
  --
  -- This provides all the information needed about
  -- the context to report the source of errors linked
  -- to this implication.
  , ic_wanted :: WantedConstraints
  -- ^ The wanteds
  , ic_status :: ImplicStatus
  }
  deriving stock (Generic)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1479
data ImplicStatus
  = -- | All wanteds in the tree are solved, all the way down
    -- See Note [Tracking redundant constraints] in GHC.Tc.Solver
    IC_Solved
  | -- | At least one insoluble Wanted constraint in the tree
    IC_Insoluble
  | -- | Neither of the above; might go either way
    IC_Unsolved
  deriving stock (Show)

-- | The "context" of an error message, e.g. "In the expression <...>",
-- "In the pattern <...>", or "In the equations for closed type family <...>".
data ErrCtxtMsg
-- ^ TODO remove?

-- | Some thing which has a type.
--
-- This datatype is used when we want to report to the user
-- that something has an unexpected type.
--
-- Similar to @TypedThing@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L467
data TypedThing
  = TypedThing'SynTermRn (SynTerm CompRn)

mkCEqCan :: Maybe TypedThing -> TcTyVar -> Tau -> Bool -> Ct
mkCEqCan thing tv ty swapped =
  CEqCan
    EqCt
      { eq_ev =
          CtWanted
            WantedCt
              { ctev_loc =
                  CtLoc
                    { ctl_origin =
                        TypeEqOrigin
                          { uo_actual = if swapped then Type'Var tv else ty
                          , uo_expected = if swapped then ty else Type'Var tv
                          , uo_thing = thing
                          }
                    , ctl_env = Nothing
                    }
              }
      , eq_lhs = tv
      , eq_rhs = ty
      }

emptyWantedConstraints :: WantedConstraints
emptyWantedConstraints =
  WantedCts
    { wc_simple = emptyBag
    , wc_impl = emptyBag
    }

instance Semigroup WantedConstraints where
  wcs1 <> wcs2 =
    WantedCts
      { wc_simple = wcs1.wc_simple <> wcs2.wc_simple
      , wc_impl = wcs1.wc_impl <> wcs2.wc_impl
      }

instance Monoid WantedConstraints where
  mempty = emptyWantedConstraints

instance Pretty' TypedThing where
  pretty' (TypedThing'SynTermRn ex) = pretty' ex

instance Pretty' CtOrigin where
  pretty' = genericPretty

instance Pretty' CtLoc where
  pretty' = genericPretty

instance Pretty' CtLocEnv where
  pretty' = genericPretty

instance Pretty' WantedCtEvidence where
  pretty' = genericPretty

instance Pretty' CtEvidence where
  pretty' = genericPretty

instance Pretty' EqCt where
  pretty' = genericPretty

instance Pretty' Ct where
  pretty' = genericPretty

instance Pretty' ImplicStatus where
  pretty' status = pretty' (show status)

instance Pretty' Implication where
  pretty' = genericPretty

instance Pretty' WantedConstraints where
  pretty' = genericPretty
