{-# LANGUAGE LambdaCase #-}

module Language.STLC.Typing.Jones2007.ConstraintTypes where

import GHC.Base (NonEmpty)
import Language.STLC.Typing.Jones2007.Bag (Bag)
import Language.STLC.Typing.Jones2007.BasicTypes (CompRn, CompTc, RealSrcSpan, SynTerm, TcLevel, TcTyVar, TcType, Type)
import Prettyprinter (Pretty (..))

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L123

-- | The "context" of an error message, e.g. "In the expression <...>",
-- "In the pattern <...>", or "In the equations for closed type family <...>".
data ErrCtxtMsg

-- | Local typechecker environment for a constraint.
--
-- Used to restore the environment of a constraint
-- when reporting errors.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L227
data CtLocEnv = CtLocEnv
  { ctl_loc :: !RealSrcSpan
  -- TODO add more data
  }

-- | Some thing which has a type.
--
-- This datatype is used when we want to report to the user
-- that something has an unexpected type.
data TypedThing
  = HsTypeRnThing (Type CompRn)
  | HsExprRnThing (SynTerm CompRn)
  | HsExprTcThing (SynTerm CompTc)

instance Pretty TypedThing where
  pretty = \case
    HsTypeRnThing ty -> pretty ty
    HsExprRnThing ex -> pretty ex
    HsExprTcThing ex -> pretty ex

-- | Constraint origin.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L491
data CtOrigin
  = -- | An application of some kind
    AppOrigin
  | -- | An annotation
    AnnOrigin
  | TypeEqOrigin
      { uo_actual :: TcType
      , uo_expected :: TcType
      , uo_thing :: Maybe TypedThing
      -- ^ The thing that has type "actual"
      }

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

-- Constraint evidence
--
-- We don't have given constraints
-- because we don't have constraints on types
-- (the <constraints> part in <constraints> => <type>)
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/constraint_kind.html
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L2166
data CtEvidence
  = CtWanted WantedCtEvidence

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

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1479
data ImplicStatus
  = -- | All wanteds in the tree are solved, all the way down
    -- See Note [Tracking redundant constraints] in GHC.Tc.Solver
    IC_Solved
  | -- | At least one insoluble Wanted constraint in the tree
    IC_Insoluble
  | -- | Neither of the above; might go either way
    IC_Unsolved

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
  , ic_skols :: NonEmpty TcTyVar
  -- ^ Introduced skolems; always skolem TcTyVars
  -- Their level numbers should be precisely ic_tclvl
  -- Their SkolemInfo should be precisely ic_info (almost)
  --       See Note [Implication invariants]
  , ic_env :: CtLocEnv
  -- ^ Records the context at the time of creation.
  --
  -- This provides all the information needed about
  -- the context to report the source of errors linked
  -- to this implication.
  , ic_wanted :: WantedConstraints
  -- ^ The wanteds
  , ic_status :: ImplicStatus
  }

-- We only have equality constraints.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L198
data Ct
  = CEqCan EqCt
  | -- | A non-canonical constraint
    --
    -- See Note [Canonicalization]
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Solver/Solve.hs#L1033
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Monad.hs#L2057
    CNonCanonical CtEvidence

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L164
type Cts = Bag Ct

-- | Constraints that we want to solve.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1089
data WantedConstraints = WantedCts
  { wc_simple :: Cts
  , wc_impl :: Bag Implication
  }
