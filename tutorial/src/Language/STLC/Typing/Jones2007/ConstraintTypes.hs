module Language.STLC.Typing.Jones2007.ConstraintTypes where

import GHC.Base (NonEmpty)
import Language.STLC.Typing.Jones2007.BasicTypes (
  SkolemInfoAnon,
  TcLevel,
  TcTyVar,
  TcType,
 )

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L123

-- | Local typechecker environment for a constraint.
--
-- Used to restore the environment of a constraint
-- when reporting errors, see `setCtLocM`.
--
-- See also 'TcLclCtxt'.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L227
data CtLocEnv = CtLocEnv

-- TODO add some data

-- { ctl_ctxt :: ![ErrCtxt]
-- , ctl_loc :: !RealSrcSpan
-- , ctl_bndrs :: !TcBinderStack
-- , ctl_tclvl :: !TcLevel
-- , ctl_in_gen_code :: !Bool
-- , ctl_rdr :: !LocalRdrEnv
-- }

-- | Constraint origin.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L491
data CtOrigin
  = -- | A given constraint from a user-written type signature. The
    -- 'SkolemInfo' inside gives more information.
    GivenOrigin SkolemInfoAnon
  | -- | An application of some kind
    AppOrigin
  | -- | An annotation
    AnnOrigin

-- | Constraint location information.
--
-- We don't need:
-- `ctl_t_or_k` because we don't have kinds.
-- `ctl_depth` because we don't have equalities involving type functions.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/CtLoc.hs#L123
data CtLoc = CtLoc
  { ctl_origin :: CtOrigin
  , ctl_env :: CtLocEnv
  -- ^ Everything we need to know about
  -- the context this Ct arose in.
  }

-- GHC uses a tree-like structure for the `Bag`.
-- A list will be enough in our case.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/Bag.hs#L48
newtype Bag a = Bag [a]

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
  -- ^ See Note [Wanteds rewrite Wanteds]
  }

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

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1408
data Implication = Implic
  { ic_tclvl :: TcLevel
  , ic_skols :: NonEmpty TcTyVar
  , ic_wanted :: WantedConstraints
  , -- TODO perhaps we only need origin?
    ic_env :: CtLocEnv
  , ic_status :: ImplicStatus
  }

-- We only have equality constraints.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L198
data Ct
  = CEqCan EqCt
  | CNonCanonical CtEvidence

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L164
type Cts = Bag Ct

-- | Constraints that we want to solve.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Constraint.hs#L1089
data WantedConstraints = WantedCts
  { wc_simple :: Cts
  , wc_impl :: Bag Implication
  }
