{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Syntax.Local.Type where

import Language.Arralac.Syntax.TTG.Type

import Data.IORef (IORef)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Typecheck.Pass (CompRn, CompTc, CompZn)
import Language.Arralac.Utils.Pretty
import Prettyprinter

type instance XVar' CompRn = RnVar
type instance XVar' CompTc = TcTyVar
type instance XVar' CompZn = ZnTyVar

-- ==============================================
-- [Variables]
-- ==============================================

-- | Similar to 'TcLevel' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L698
--
-- Also see Note [TcLevel invariants] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L705
newtype TcLevel = TcLevel Int
  deriving newtype (Show, Eq, Ord, Num)

-- | Expected type.
--
-- Similar to 'ExpType' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L401
data Expected a = Infer (IORef a) | Check a

-- TODO ^ Do we need TcLevel in Infer?
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L418

-- | A term or a type variable produced by the renamer.
--
-- Similar to 'RdrName' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Reader.hs#L166
data RnVar = RnVar
  { varName :: !Name
  }

-- | A bound type variable that can only appear in a 'forall'.
--
-- @forall {bound tvs}. {type body}@
--
-- Such variables are either skolemised or instantiated and never appear in type bodies.
type TcBoundVar = TcTyVar

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

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L163
type TyVar = RnVar

-- | Type variable that is a metavariable.
--
-- Requires explicit checks in function definitions.
type TcTyVarMeta = TcTyVar

type TcTyVarSkolem = TcTyVar

-- | A term variable, possibly with a known type.
data TcTermVar
  = TcTermVar
  { varName :: !Name
  , varType :: Expected TcType
  }

-- | A zonked type variable.
--
-- Doesn't have a type, unlike 'Id' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L277
data ZnTyVar
  = -- | Variable identifier
    -- Always local and vanilla.
    ZnTyVar
    { varName :: !Name
    }

data ZnTermVar
  = -- | Variable identifier
    -- Always local and vanilla.
    ZnTermVar
    { varName :: !Name
    , varType :: Type CompZn
    }

-- | Similar to 'MetaDetails' in GHC.
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

-- | Details about a 'TyVar'
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
    -- The original definition of TyVar had a BoundTv constructor (p.41).
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
-- [Types]
-- ==============================================

type Sigma = TcType
type Rho = TcType -- No top-level ForAll
type Tau = TcType -- No ForAlls anywhere

-- | A type that can have mutable type variables.
--
-- Similar to 'TcType' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L346
--
-- GHC also has 'Kind's, but we don't have them.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L110
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L107
type TcType = Type CompTc

type RnType = Type CompRn

type ZnType = Type CompZn

type TcTypeMeta = TcType

-- ==============================================
-- [Concrete types]
-- ==============================================

data Concrete = Concrete
  { concreteName :: Name
  , concreteType :: TypeConcrete
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

-- ==============================================
-- [Annotations in the AST]
-- ==============================================

data TcAnno = TcAnno
  { annoSrcLoc :: SrcSpan
  , annoType :: Expected TcType
  }

data ZnAnno = ZnAnno
  { annoSrcLoc :: SrcSpan
  , annoType :: ZnType
  }

-- ==============================================
-- [Instances for Types]
-- ==============================================

instance Pretty' Concrete where
  pretty' c = pretty' c.concreteType

instance (Pretty' a) => Pretty' (Expected a) where
  pretty' (Infer _) = "[Infer]"
  pretty' (Check a) = "[Check]: " <> pretty' a

instance Pretty' SkolemInfoAnon where
  pretty' = \case
    SigSkol _ _ _ -> "SigSkol"
    SigTypeSkol _ -> "SigTypeSkol"
    ForAllSkol _ -> "ForAllSkol"
    InferSkol _ -> "InferSkol"
    UnifyForAllSkol _ -> "UnifyForAllSkol"

-- ==============================================
-- [Instances for CompRn types]
-- ==============================================

instance Eq RnVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord RnVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Pretty' RnVar where
  pretty' var = pretty' var.varName

-- ==============================================
-- [Instances for CompTc types]
-- ==============================================

instance Eq TcTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord TcTyVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Pretty' TcLevel where
  pretty' (TcLevel lvl) = "L" <+> pretty' lvl

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

instance Pretty' TcTermVar where
  pretty' var =
    hsep
      [ pretty' var.varName
      , "::"
      , braces (pretty' var.varType)
      ]

parensIndent :: Doc ann -> Doc ann
parensIndent x = parens (line <> indent 2 x <> line)

-- ==============================================
-- [Instances for Zn (zonking pass) things]
-- ==============================================

instance Eq ZnTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord ZnTyVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Pretty' ZnTyVar where
  pretty' ZnTyVar{varName} = pretty' varName

instance Pretty' ZnTermVar where
  pretty' ZnTermVar{varName, varType} = parens (pretty' varName <+> "::" <+> pretty' varType)

instance (Pretty' (XVar' p)) => Pretty' (Type p) where
  pretty' = \case
    Type'Var var -> pretty' var
    Type'ForAll vars body -> "forall " <> hsep (pretty' <$> vars) <> "." <+> pretty' body
    Type'Fun arg res -> parens' arg' <+> "->" <+> pretty' res
     where
      arg' = pretty' arg
      parens' =
        case arg of
          Type'Fun _ _ -> parens
          Type'ForAll _ _ -> parens
          _ -> id
    Type'Concrete ty -> pretty' ty
