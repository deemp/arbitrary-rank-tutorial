module Language.Arralac.Typecheck.Constraints where

import GHC.Generics (Generic)
import Language.Arralac.Typecheck.Bag (Bag (..), emptyBag)
import Language.Arralac.Typecheck.BasicTypes (CompRn, Pretty' (..), RealSrcSpan, SynTerm, TcLevel, TcTyVar, TcType)
import Language.Arralac.Typecheck.Pretty (genericPretty)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L123

-- | The "context" of an error message, e.g. "In the expression <...>",
-- "In the pattern <...>", or "In the equations for closed type family <...>".
--
-- TODO remove?
data ErrCtxtMsg

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

-- | Some thing which has a type.
--
-- This datatype is used when we want to report to the user
-- that something has an unexpected type.
data TypedThing = HsExprRnThing (SynTerm CompRn)

-- | Constraint origin.
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

-- | Constraint location information.
--
-- We don't need:
-- `ctl_t_or_k` because we don't have kinds.
-- `ctl_depth` because we don't have equalities involving type functions.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L123
data CtLoc = CtLoc
  { ctl_origin :: CtOrigin
  , -- TODO remove Maybe
    ctl_env :: Maybe CtLocEnv
  -- ^ Everything we need to know about
  -- the context this Ct arose in.
  }
  deriving stock (Generic)

-- | Evidence for a Wanted constraint
--
-- We want to keep things simple,
-- so we don't rewrite wanteds.
--
-- See Note [Wanteds rewrite Wanteds]
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L2415
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L2178
data WantedCtEvidence
  = WantedCt
  { ctev_loc :: CtLoc
  }
  deriving stock (Generic)

-- Constraint evidence
--
-- We don't have given constraints
-- because we don't have constraints on types
-- (the <constraints> part in <constraints> => <type>)
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/constraint_kind.html
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L2166
data CtEvidence = CtWanted
  { ct_ev_wanted :: WantedCtEvidence
  }
  deriving stock (Generic)

-- TODO store how a constrain appeared
-- For equality, store previous constraints

-- TODO
-- Do we have given equality constraints?

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L299
data EqCt = EqCt
  { eq_ev :: CtEvidence
  , eq_lhs :: TcTyVar
  , eq_rhs :: TcType
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

-- Out of simple constraints (see `WantedConstraints.wc_simple`), we only have equality constraints.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L198
data Ct = CEqCan
  { ct_eq_can :: EqCt
  --  ^ A canonical equality constraint.
  --
  -- See Note [Canonicalization]
  --
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Solver/Solve.hs#L1033
  --
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Monad.hs#L2057
  }
  deriving stock (Generic)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L164
type Cts = Bag Ct

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L164
type Impls = Bag Implication

-- | Constraints that we want to solve.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1089
data WantedConstraints = WantedCts
  { wc_simple :: Cts
  , wc_impl :: Impls
  }
  deriving stock (Generic)

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
  pretty' (HsExprRnThing ex) = pretty' ex

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
