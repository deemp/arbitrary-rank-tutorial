{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Syntax.Local.TyVar.Tc where

import Data.IORef (IORef)
import Data.Text
import GHC.Generics
import Language.Arralac.Pass.Types
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Unique
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.TyVar
import Language.Arralac.Syntax.TTG.Type (Type)
import Language.Arralac.Typechecker.Types
import Prettyprinter

type instance XTyVar CompTc = TcTyVar

-- | A type variable.
--
-- For type safety, this is a separate type instead of a type synonym.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L169
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L268
data TcTyVar
  = -- | Used only during type inference
    TcTyVar
    { varName :: !Name
    , varDetails :: TcTyVarDetails
    }
  deriving stock (Generic)

-- | A bound type variable that can only appear in a 'forall'.
--
-- @forall {bound tvs}. {type body}@
--
-- Such variables are either skolemised or instantiated and never appear in type bodies.
type TcBoundVar = TcTyVar

-- | Type variable that is a metavariable.
--
-- Requires explicit checks in function definitions.
type TcTyVarMeta = TcTyVar

type TcTyVarSkolem = TcTyVar

-- | Similar to @MetaDetails@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L634
data MetaDetails
  = -- | Flexi type variables unify to become Indirects.
    --
    -- 'Flexi' means that the type variable is unfilled.
    --
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L915
    Flexi
  | Indirect TcType

-- TODO ^ add another Indirect variant for types zonked during typechecking.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15552#note_159240
--
-- The issue was mentioned here:
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Zonk/Type.hs#L260

-- | What restrictions are on this metavariable around unification?
-- These are checked in GHC.Tc.Utils.Unify.checkTopShape
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L640
data MetaInfo
  = -- | This MetaTv is an ordinary unification variable
    -- A TauTv is always filled in with a tau-type, which
    -- never contains any ForAlls.
    TauTv
  | -- | A variant of TauTv, except that it should not be
    --   unified with a type, only with a type variable
    --
    -- See Note [TyVarTv] in GHC.Tc.Utils.TcMType
    --
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L698
    TyVarTv

-- | Details about a type variable.
--
-- See Note [TyVars and TcTyVars during type checking]
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L601
data TcTyVarDetails
  = -- | A type variable always bound by an enclosing 'forall'.
    --
    -- See @Practical type inference for arbitrary-rank types@.
    --
    -- No well-formed Type ever has a free BoundTv. (p.42)
    --
    -- Deep skolemisation doesn't affect argument sigmas, only result ones (p. 24).
    --
    -- We don't have deep instantiation (p. 28).
    --
    -- The original definition of @TyVar@ had a @BoundTv@ constructor (p.41).
    --
    -- Hence, we should be able to represent bound type variables during typechecking.
    --
    -- Never a unification variable:
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1797
    BoundTv
      { tcLevel :: TcLevel
      -- TODO store the info about the binding site, including the id of the binder?
      -- , binderName :: Name
      }
  | -- | A skolem.
    -- A constant of an unknown type. Never bound by a ForAll and can be free in a Type.
    SkolemTv
      { skolemInfo :: SkolemInfo
      -- ^ See Note [Keeping SkolemInfo inside a SkolemTv]
      , tcLevel :: TcLevel
      -- ^ Level of the implication that binds it
      -- See GHC.Tc.Utils.Unify Note [Deeper level on the left] for
      --     how this level number is used
      }
  | -- | An unknown monotype.
    -- Never quantified by a ForAll. (p. 42)
    MetaTv
      { metaTvInfo :: MetaInfo
      , metaTvRef :: IORef MetaDetails
      , tcLevel :: TcLevel
      -- ^ See Note [TcLevel invariants]
      }

-- ==============================================
-- [SkolemInfo]
-- ==============================================

-- | UserTypeCtxt describes the origin of the polymorphic type
-- in the places where we need an expression to have that type.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L98
data UserTypeCtxt
  = FunSigCtxt -- Function type signature, when checking the type
      Name -- Name of the function
  | InfSigCtxt Name -- Inferred type for function
  | ExprSigCtxt -- Expression type signature
  | TyVarBndrKindCtxt Name -- The kind of a type variable being bound

-- | 'SkolemInfoAnon' stores the origin of a skolem type variable (e.g. bound by
-- a user-written forall, the header of a data declaration, a deriving clause, ...).
--
-- This information is displayed when reporting an error message, such as
--
--  @"Couldn't match 'k' with 'l'"@
--
-- This allows us to explain where the type variable came from.
--
-- When several skolem type variables are bound at once, prefer using 'SkolemInfo',
-- which stores a 'Unique' which allows these type variables to be reported
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L266
data SkolemInfoAnon
  = SigSkol -- A skolem that is created by instantiating
  -- a programmer-supplied type signature
  -- Location of the binding site is on the TyVar
  -- See Note [SigSkol SkolemInfo]
      UserTypeCtxt -- What sort of signature
      (Type CompRn) -- Original type signature (before skolemisation)
      -- TODO why list?
      [(Name, TcType)] -- Maps the original name of the skolemised tyvar
      -- to its instantiated version
  | SigTypeSkol UserTypeCtxt
  | -- like SigSkol, but when we're kind-checking the *type*
    -- hence, we have less info
    ForAllSkol -- Bound by a user-written "forall".
      [TcTyVar] -- Shows just the binders
  | InferSkol [(Name, TcType)]
  | -- We have inferred a type for these (mutually recursive)
    -- polymorphic Ids, and are now checking that their RHS
    -- constraints are satisfied.
    UnifyForAllSkol -- We are unifying two for-all types
      TcType -- The instantiated type *inside* the forall

-- | 'SkolemInfo' stores the origin of a skolem type variable,
-- so that we can display this information to the user in case of a type error.
--
-- The 'Unique' field allows us to report all skolem type variables bound in the
-- same place in a single report.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L246
data SkolemInfo
  = SkolemInfo
      -- | The Unique is used to common up skolem variables bound
      --   at the same location (only used in pprSkols)
      Unique
      -- | The information about the origin of the skolem type variable
      SkolemInfoAnon

-- TODO ^ actually use this

instance Eq TcTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord TcTyVar where
  var1 <= var2 = var1.varName <= var2.varName

getTcTyVarDetails :: TcTyVarDetails -> Text
getTcTyVarDetails = \case
  BoundTv{} -> "Bound"
  SkolemTv{} -> "Skolem"
  MetaTv{} -> "Meta"

instance Pretty' TcTyVar where
  pretty' var =
    prettyCompact var.varName
      <> case ?prettyVerbosity of
        PrettyVerbosity'User -> mempty
        _ ->
          encloseSep
            lbracket
            rbracket
            (comma <> space)
            ( case ?prettyVerbosity of
                PrettyVerbosity'Normal ->
                  [ pretty' var.varDetails.tcLevel
                  , pretty' $ getTcTyVarDetails var.varDetails
                  ]
                PrettyVerbosity'Detailed ->
                  [ "ID" <+> pretty' var.varName.nameUnique
                  , pretty' var.varDetails.tcLevel
                  , pretty' $ getTcTyVarDetails var.varDetails
                  , pretty' var.varName.nameLoc
                  ]
                PrettyVerbosity'User -> []
            )

-- TODO provide more info
instance Pretty' SkolemInfoAnon where
  pretty' = \case
    SigSkol _ _ _ -> "SigSkol"
    SigTypeSkol _ -> "SigTypeSkol"
    ForAllSkol _ -> "ForAllSkol"
    InferSkol _ -> "InferSkol"
    UnifyForAllSkol _ -> "UnifyForAllSkol"
