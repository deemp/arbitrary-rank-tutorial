{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.STLC.Typing.Jones2007.BasicTypes where

-- This module defines the basic types used by the type checker
-- Everything defined in here is exported

import Data.Data (Data (..))
import Data.Function ((&))
import Data.IORef
import Data.Map (Map)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Language.STLC.Common ()
import Prettyprinter
import Prelude hiding ((<>))

-----------------------------------
--      Architecture             --
-----------------------------------
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core.hs#L138

-----------------------------------
--      Expressions             --
-----------------------------------
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L332
data SynTerm x
  = -- | x
    -- TODO replace with a family
    SynTerm'Var (XSynTerm'Var' x) (XSynTerm'Var x)
  | -- | 3
    SynTerm'Lit (XSynTerm'Lit' x) (XSynTerm'Lit x)
  | -- | f x
    SynTerm'App (XSynTerm'App' x) (XSynTerm'App'Fun x) (XSynTerm'App'Arg x)
  | -- | \ x -> x
    SynTerm'Lam (XSynTerm'Lam' x) (XSynTerm'Lam'Var x) (XSynTerm'Lam'Body x)
  | -- | \ (x :: a) -> x
    SynTerm'ALam (XSynTerm'ALam' x) (XSynTerm'ALam'Var x) (XSynTerm'ALam'Type x) (XSynTerm'ALam'Body x)
  | -- | let x = f y in x+1
    SynTerm'Let (XSynTerm'Let' x) (XSynTerm'Let'Name x) (XSynTerm'Let'AssignedTerm x) (XSynTerm'Let'InTerm x)
  | -- | (f x) :: Int
    SynTerm'Ann (XSynTerm'Ann' x) (XSynTerm'Ann'Term x) (XSynTerm'Ann'Type x)

-- TODO add parenthesized expressions
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L382

-- TODO are concrete types represented as `Name`s?
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Pat.hs#L181

-- TODO
-- In SynTerm GhcTc, XSynTerm'Var' x resolves to NoFieldExt
-- For other fields, the extension field is used to store the inferred type

-- TODO add extension field
-- TODO explain what it can be used for
-- XXExprGhcRn
-- XXExprGhcTc
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Expr.hs#L1294

-- TODO uncomment
-- -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Type.hs#L812
data SynType x
  = -- | Type variable
    --
    -- @x@
    SynType'Var (XSynType'Var' x) (XSynType'Var x)
  | -- | Forall
    --
    -- @forall a. b@
    SynType'ForAll (XSynType'ForAll' x) (XSynType'ForAll'Vars x) (XSynType'ForAll'Body x)
  | -- | Function
    --
    -- @a -> b@
    SynType'Fun (XSynType'Fun' x) (XSynType'Fun'Arg x) (XSynType'Fun'Res x)
  | -- | Concrete type
    --
    -- @String@
    SynType'Concrete (XSynType'Concrete' x) (XSynType'Concrete x)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Lit.hs#L48
data SynLit
  = SynLit'Num Integer
  | SynLit'Bool Bool
  | SynLit'Str FastString

-- TODO add case to support Trees That Grow
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L537

---------------- Terms ---------------------

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L412
type family XSynTerm'Var' x
type family XSynTerm'Var x

type family XSynTerm'Lit' x
type family XSynTerm'Lit x

type family XSynTerm'App' x
type family XSynTerm'App'Fun x
type family XSynTerm'App'Arg x

type family XSynTerm'Lam' x
type family XSynTerm'Lam'Var x
type family XSynTerm'Lam'Body x

type family XSynTerm'ALam' x
type family XSynTerm'ALam'Var x
type family XSynTerm'ALam'Type x
type family XSynTerm'ALam'Body x

type family XSynTerm'Let' x
type family XSynTerm'Let'Name x
type family XSynTerm'Let'AssignedTerm x
type family XSynTerm'Let'InTerm x

type family XSynTerm'Ann' x
type family XSynTerm'Ann'Term x
type family XSynTerm'Ann'Type x

---------------- Literals ---------------------

type family XSynLit'Num' x
type family XSynLit'Num x

type family XSynLit'Str' x
type family XSynLit'Str x

---------------- Types (syntactic) ---------------------

type family XSynType'Var' x
type family XSynType'Var x

type family XSynType'ForAll' x
type family XSynType'ForAll'Vars x
type family XSynType'ForAll'Body x

type family XSynType'Fun' x
type family XSynType'Fun'Arg x
type family XSynType'Fun'Res x

type family XSynType'Concrete' x
type family XSynType'Concrete x

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L169
data Pass = Renamed | Typechecked | Zonked
  deriving stock (Data)

-- | Used as a data type index for the hsSyn AST; also serves
-- as a singleton type for Pass
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L157
data CompPass (c :: Pass) where
  CompRn :: CompPass 'Renamed
  CompTc :: CompPass 'Typechecked
  CompZn :: CompPass 'Zonked

-- Type synonyms as a shorthand for tagging
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L173
type CompRn = CompPass 'Renamed -- Output of renamer
type CompTc = CompPass 'Typechecked -- Output of typechecker
type CompZn = CompPass 'Zonked -- Output of zonker that by construction doesn't contain metavariables

-- | Maps the "normal" id type for a given GHC pass
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L205
type family IdCompP pass where
  IdCompP 'Renamed = Name
  IdCompP 'Typechecked = TcTyVar
  IdCompP 'Zonked = Id

-- TODO use ping-pong

-- Note [Ping-pong in TTG]
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Why do we annotate some nodes without using the extension field?
--
-- See
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L125
-- and
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/handling-source-locations#ping-pong-style
--
-- > If this were done with the constructor extension point of TTG,
-- then one would lose some type safety:
-- There would no longer be a guaruntee that there will always
-- be a Located layer between the Expr layers in our huge expression sandwich.

data GenLocated l e = L l e
  deriving stock (Eq, Ord, Show, Data, Functor, Foldable, Traversable)

type Located = GenLocated SrcSpan

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L120
type family XRec p a = r | r -> a

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L101
-- TODO why XAnno doesn't use pass
type instance XRec (CompPass p) a = XAnno a

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L210
type LIdCompP p = XAnno (IdCompP p)

-- | We attach SrcSpans to lots of things, so let's have a datatype for it.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L759
data Annotated l e = Annotated l e
  deriving stock (Eq, Ord, Show, Data, Functor, Foldable, Traversable)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L122
type family Anno a = b

-- (XAnno tree) wraps `tree` in a Compiler-specific,
-- but pass-independent, source location
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L105
type XAnno a = Annotated (Anno a) a

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L167
type family IdP p

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L169
type XVar p = XRec p (IdP p)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L202
type instance IdP (CompPass p) = IdCompP p

-- See Note [Ping-pong in TTG]
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L647
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L108
type instance Anno Name = SrcSpan
type instance Anno SynLit = SrcSpan
type instance Anno TcTyVar = SrcSpan

-- ----------------------

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L144
data NameSpace
  = -- | Terms namespace.
    NameSpace'Term
  | -- | Types namespace.
    NameSpace'Type'Var
  | -- | Built-in types namespace.
    NameSpace'Type'Concrete
  deriving stock (Eq, Show)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/FastString.hs#L211
type FastString = Text

-- | Occurrence Name
--
-- In this context that means:
-- "classified (i.e. as a type name, value name, etc) but not qualified
-- and not yet resolved"
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L360
data OccName = OccName
  { occNameSpace :: !NameSpace
  , occNameFS :: !FastString
  }
  deriving stock (Show)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L399
data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulGenerated
  | UnhelpfulOther !FastString
  deriving stock (Eq, Show)

-- | Real Source Span
--
-- It's open on the right: [start; finish)
--
-- It's zero-based here. Not sure about GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L367
data RealSrcSpan
  = RealSrcSpan'
  { srcSpanFile :: !FastString
  , srcSpanSLine :: {-# UNPACK #-} !Int
  , srcSpanSCol :: {-# UNPACK #-} !Int
  , srcSpanELine :: {-# UNPACK #-} !Int
  , srcSpanECol :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq)

-- | Source Span
--
-- A 'SrcSpan' identifies either a specific portion of a text file
-- or a human-readable description of a location.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L392
data SrcSpan
  = -- TODO Replace with RealSrcSpan
    RealSrcSpan RealSrcSpan
  | UnhelpfulSpan UnhelpfulSpanReason

-- | The key type representing kinds in the compiler.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L110
type Kind = Type

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L107
type KindOrType = Type

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L163
type TyVar = RnVar

type Sigma = TcType
type Rho = TcType -- No top-level ForAll
type Tau = TcType -- No ForAlls anywhere

data TypeConcrete
  = TypeConcrete'Int
  | TypeConcrete'Bool
  | TypeConcrete'String
  deriving stock (Eq)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L124
data Type p
  = -- | Vanilla type variable
    Type'Var (XVar' p)
  | Type'ForAll
      [XVar' p]
      (Type p)
  | -- | This is a special case of a type constructor
    -- where the type constructor is (->).
    Type'Fun (Type p) (Type p)
  | -- | Type literals are similar to type constructors.
    Type'Concrete TypeConcrete

-- TODO implement
instance (Show (XVar' p)) => Show (Type p) where
  show = \case
    Type'Var var -> show var
    Type'ForAll vars body -> "forall " <> concatMap show vars <> ". " <> show body
    Type'Fun arg res -> "(" <> show arg <> ")" <> "->" <> "(" <> show res <> ")"
    Type'Concrete ty -> show ty

-- TODO separate Type from TcType
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L576
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Zonk/Type.hs#L260
-- Possible design: two types of Indirects
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15552#note_158972
-- + Separate TcType from Type
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15552#note_159240

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L346
type TcType = Type CompTc

type RnType = Type CompRn

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique.hs#L98
type Unique = Int

type ZnType = Type CompZn

-------------------------------------

-- | UserTypeCtxt describes the origin of the polymorphic type
-- in the places where we need an expression to have that type
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L98
data UserTypeCtxt
  = FunSigCtxt -- Function type signature, when checking the type
      Name -- Name of the function
  | InfSigCtxt Name -- Inferred type for function
  | ExprSigCtxt -- Expression type signature
  | TyVarBndrKindCtxt Name -- The kind of a type variable being bound

-- | Type variable that is a metavariable.
-- requires explicit checks in function definitions.
type TcTyVarMeta = TcTyVar

type TcTyVarSkolem = TcTyVar

type TcTypeMeta = TcType

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

type family XVar' p

type instance XVar' CompRn = RnVar
type instance XVar' CompTc = TcTyVar
type instance XVar' CompZn = ZnTyVar

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L634
data MetaDetails
  = -- | Flexi type variables unify to become Indirects
    -- Means that the type variable is unfilled
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L915
    Flexi
  | Indirect TcType

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L698
newtype TcLevel = TcLevel Int
  deriving newtype (Show, Eq, Ord, Num)

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
    -- See Note [TyVarTv] in GHC.Tc.Utils.TcMType
    TyVarTv

-- A TyVarDetails is inside a TyVar
-- See Note [TyVars and TcTyVars during type checking]
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L601
data TcTyVarDetails
  = -- Always bound by an enclosing ForAll. No well-formed Type ever has a free BoundTv. (p.42)

    -- TODO -- A tyvar binder is never a unification variable (TauTv),
    -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1797

    -- | Deep skolemisation doesn't affect argument sigmas, only result ones (p. 24).
    -- We don't have deep instantiation (p. 28).
    -- The original definition of TyVar had a BoundTv constructor (p.41).
    -- Hence, we should be able to represent bound tyvars during typechecking.
    BoundTv
      { tcLevel :: TcLevel
      -- TODO store the info about the binding site, including the id of the binder?
      -- , binderName :: Name
      }
  | -- A skolem
    -- A constant of an unknown type. Never bound by a ForAll and can be free in a Type.
    SkolemTv
      { skolemInfo :: SkolemInfo
      -- ^ See Note [Keeping SkolemInfo inside a SkolemTv]
      , tcLevel :: TcLevel
      -- ^ Level of the implication that binds it
      -- See GHC.Tc.Utils.Unify Note [Deeper level on the left] for
      --     how this level number is used
      }
  | -- An unknown monotype.
    -- Never quantified by a ForAll. (p. 42)
    MetaTv
      { metaTvInfo :: MetaInfo
      , metaTvRef :: IORef MetaDetails
      , tcLevel :: TcLevel
      -- ^ See Note [TcLevel invariants]
      }

-- | Identifier
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L150
type Id = ZnTyVar

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L126
data Name = Name
  { nameOcc :: OccName
  , nameUnique :: {-# UNPACK #-} !Unique
  , nameLoc :: !SrcSpan
  -- , nameSort :: NameSort
  -- See https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L148
  }

-- TODO use a single Var type and select varDetails depending on the phase?
-- E.g., In CompRn, varDetails is Void

-- | Variable
--
-- Essentially a typed 'Name', that may also contain some additional information
-- about the 'Var' and its use sites.
--
-- Variable is always local
--
-- FIXME If TyVar occurs during constraint solving, it means BoundTv
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L257
data RnVar = RnVar
  { varName :: !Name
  }

-- TODO What happens to bound type variables?
-- They are skolemised (`skolemise`, `newSkolemTyVar`) or instantiated (`instantiate`) with metavariables
-- Can we immediately replace them with TcTyVar?

type TcBoundVar = TcTyVar

-- | Type variable that might be a metavariable.
-- A separate type instead of a type synonym
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L169
-- and a constructor
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L268
data TcTyVar
  = -- | Used only during type inference
    TcTyVar
    { varName :: !Name
    , varDetails :: TcTyVarDetails
    }

-- TODO write error in each vartype?
-- so that it must be filled
data TcTermVar
  = TcTermVar
  { varName :: !Name
  , varType :: Expected TcType
  }

-- Zonked type variable or a term variable with a zonked type
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

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Module/Name.hs#L13
newtype ModuleName = ModuleName Text deriving newtype (Show, Eq)

-- TODO specify instances for each phase
-- TODO store instances in a separate module

---------------- Terms ---------------------

-- TODO might need TcLevel
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L418
data Expected a = Infer (IORef a) | Check a

data AnnoTc = AnnoTc
  { annoSrcLoc :: SrcSpan
  , annoType :: Expected TcType
  }

data AnnoZn = AnnoZn
  { annoSrcLoc :: SrcSpan
  , annoType :: ZnType
  }

data Concrete = Concrete
  { concreteName :: Name
  , concreteType :: TypeConcrete
  }

-- TODO Assume we need the same equations for all variables
-- Then, we can use another to provide them in that family
type family XSynTerm'VarCommon x
type family XSynTerm'AnnoCommon x

type instance XSynTerm'VarCommon CompRn = Name
type instance XSynTerm'VarCommon CompTc = TcTermVar
type instance XSynTerm'VarCommon CompZn = ZnTermVar

type instance XSynTerm'AnnoCommon CompRn = SrcSpan
type instance XSynTerm'AnnoCommon CompTc = AnnoTc
type instance XSynTerm'AnnoCommon CompZn = AnnoZn

-- TODO move

-- TODO is this true?
-- Seems like we can use the same representation
-- for type-level and term-level variables
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L205
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L334
-- Answer: it's not true
-- In our calculus, term-level variables don't have types

-- TODO move instances to a separate module
type instance XSynTerm'Var' x = ()
type instance XSynTerm'Var x = XSynTerm'VarCommon x

type instance XSynTerm'Lit' x = XSynTerm'AnnoCommon x
type instance XSynTerm'Lit x = SynLit

type instance XSynTerm'App' x = XSynTerm'AnnoCommon x
type instance XSynTerm'App'Fun x = SynTerm x
type instance XSynTerm'App'Arg x = SynTerm x

type instance XSynTerm'Lam' x = XSynTerm'AnnoCommon x
type instance XSynTerm'Lam'Var x = XSynTerm'VarCommon x
type instance XSynTerm'Lam'Body x = SynTerm x

type instance XSynTerm'ALam' x = XSynTerm'AnnoCommon x
type instance XSynTerm'ALam'Var x = XSynTerm'VarCommon x
type instance XSynTerm'ALam'Type x = SynType x
type instance XSynTerm'ALam'Body x = SynTerm x

type instance XSynTerm'Let' x = XSynTerm'AnnoCommon x
type instance XSynTerm'Let'Name x = XSynTerm'VarCommon x
type instance XSynTerm'Let'AssignedTerm x = SynTerm x
type instance XSynTerm'Let'InTerm x = SynTerm x

type instance XSynTerm'Ann' x = XSynTerm'AnnoCommon x
type instance XSynTerm'Ann'Term x = SynTerm x
type instance XSynTerm'Ann'Type x = SynType x

---------------- Literals ---------------------

type instance XSynLit'Num' x = ()

---------------- Types (syntactic) ---------------------

type instance XSynType'Var' x = ()

-- TODO note about the same representations
-- of type-level and term-level variables
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L334
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Type.hs#L828
type instance XSynType'Var CompRn = Name
type instance XSynType'Var CompTc = TcTyVar
type instance XSynType'Var CompZn = ZnTyVar

type instance XSynType'ForAll' x = SrcSpan
type instance XSynType'ForAll'Vars CompRn = [Name]
type instance XSynType'ForAll'Vars CompTc = [TcTyVar]
type instance XSynType'ForAll'Vars CompZn = [ZnTyVar]
type instance XSynType'ForAll'Body x = SynType x

type instance XSynType'Fun' x = SrcSpan
type instance XSynType'Fun'Arg x = SynType x
type instance XSynType'Fun'Res x = SynType x

-- TODO explain when to use annotations and when not to
type instance XSynType'Concrete' x = ()
type instance XSynType'Concrete CompRn = Name
type instance XSynType'Concrete CompTc = Concrete
type instance XSynType'Concrete CompZn = Concrete

type NameFs = FastString

type IUniqueSupply = (?uniqueSupply :: IORef Int)

-- | Current scope.
--
-- Visible variable names and their ids.
type IScope = (?scope :: Map NameFs Int)

type ICurrentFilePath = (?currentFilePath :: FastString)

type IDebug = (?debug :: Bool)

instance Eq Name where
  n1 == n2 = n1.nameUnique == n2.nameUnique

instance Ord Name where
  n1 <= n2 = n1.nameUnique <= n2.nameUnique

instance Pretty RealSrcSpan where
  pretty r =
    (pretty r.srcSpanFile <> ":")
      <> (pretty (r.srcSpanSLine + 1) <> ":" <> pretty (r.srcSpanSCol + 1))
      <> "-"
      <> (pretty (r.srcSpanELine + 1) <> ":" <> pretty (r.srcSpanECol + 1))

instance Show RealSrcSpan where
  show = show . pretty

instance Pretty SkolemInfoAnon where
  pretty = \case
    SigSkol _ _ _ -> "SigSkol"
    SigTypeSkol _ -> "SigTypeSkol"
    ForAllSkol _ -> "ForAllSkol"
    InferSkol _ -> "InferSkol"
    UnifyForAllSkol _ -> "UnifyForAllSkol"

instance Show SkolemInfoAnon where
  show = show . pretty

instance Pretty UnhelpfulSpanReason where
  pretty = \case
    UnhelpfulNoLocationInfo -> "No location"
    UnhelpfulGenerated -> "Generated"
    UnhelpfulOther reason -> pretty reason

instance Pretty SrcSpan where
  pretty = \case
    RealSrcSpan sp -> pretty sp
    UnhelpfulSpan reason -> "Unknown span:" <+> pretty reason

instance Show SrcSpan where
  show = show . pretty

instance Pretty Name where
  pretty name =
    case name.nameOcc.occNameSpace of
      NameSpace'Type'Concrete -> pretty name.nameOcc.occNameFS
      _ -> pretty name.nameOcc.occNameFS <> "_" <> pretty name.nameUnique

instance Show Name where
  show n =
    T.unpack n.nameOcc.occNameFS <> "[" <> "ID " <> show n.nameUnique <> ", " <> show n.nameLoc <> "]"

instance Pretty SynLit where
  pretty = \case
    SynLit'Num val -> pretty val
    SynLit'Str val -> "\"" <> pretty val <> "\""
    SynLit'Bool val -> pretty val

instance Pretty TypeConcrete where
  pretty = \case
    TypeConcrete'Int -> "Int"
    TypeConcrete'Bool -> "Bool"
    TypeConcrete'String -> "String"

instance Show TypeConcrete where
  show = show . pretty

instance Pretty Concrete where
  pretty c = pretty c.concreteType

instance (Pretty a) => Pretty (Expected a) where
  pretty (Infer _) = "[Infer]"
  pretty (Check a) = "[Check]: " <> pretty a

-- ========================================
-- Instances for Rn (renaming stage) things
-- ========================================

instance Eq RnVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord RnVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Show RnVar where
  show var = show var.varName

instance Pretty RnVar where
  pretty var = pretty var.varName

instance Pretty (Type CompRn) where
  pretty = \case
    Type'Var var -> pretty var
    Type'ForAll vars body -> "forall" <+> hsep (pretty <$> vars) <> "." <+> pretty body
    Type'Fun arg res -> "(" <> pretty arg <> ")" <> "->" <> "(" <> pretty res <> ")"
    Type'Concrete ty -> pretty ty

instance Pretty (SynType CompRn) where
  pretty = \case
    SynType'Var _ var -> pretty var
    SynType'ForAll _ vars ty -> "forall" <+> hsep (pretty <$> vars) <> "." <+> pretty ty
    SynType'Fun _ ty1 ty2 -> pretty ty1 <+> "->" <+> pretty ty2
    SynType'Concrete _ ty -> pretty ty

instance Pretty (SynTerm CompRn) where
  pretty = \case
    SynTerm'Var _ var -> pretty var
    SynTerm'Lit _ val -> pretty val
    SynTerm'App _ term1 term2 -> parens (pretty term1) <+> pretty term2
    SynTerm'Lam _ var term -> "\\" <> pretty var <> "." <+> pretty term
    SynTerm'ALam _ var ty term -> "\\" <> parens (pretty var <+> "::" <+> pretty ty) <> "." <+> pretty term
    SynTerm'Let _ name term1 term2 -> "let" <+> pretty name <+> "=" <+> pretty term1 <+> "in" <+> pretty term2
    SynTerm'Ann _ term ty -> parens (parens (pretty term) <+> "::" <+> pretty ty)

-- ============================================
-- Instances for Tc (typechecking stage) things
-- ============================================

instance Eq TcTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord TcTyVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Pretty TcLevel where
  pretty (TcLevel lvl) = "L " <> pretty lvl

getTcTyVarKind :: (IsString a) => TcTyVarDetails -> a
getTcTyVarKind = \case
  BoundTv{} -> "Bound"
  SkolemTv{} -> "Skolem"
  MetaTv{} -> "Meta"

instance Pretty TcTyVar where
  pretty v =
    pretty v.varName
      <> brackets
        ( pretty (show v.varDetails.tcLevel)
            <> ","
            <+> getTcTyVarKind v.varDetails
        )

instance Show TcTyVar where
  show var =
    T.unpack var.varName.nameOcc.occNameFS
      <> "["
      <> ("L " <> show var.varDetails.tcLevel <> ", ")
      <> (getTcTyVarKind var.varDetails <> ", ")
      <> ("ID " <> show var.varName.nameUnique <> ", ")
      <> show var.varName.nameLoc
      <> "]"

instance Pretty TcTermVar where
  pretty var =
    hsep
      [ pretty var.varName
      , "::"
      , braces (pretty var.varType)
      ]

instance Pretty (Type CompTc) where
  pretty = \case
    Type'Var var -> pretty var
    Type'ForAll vars body -> "forall" <+> hcat (pretty <$> vars) <> "." <+> pretty body
    Type'Fun arg res -> "(" <> pretty arg <> ")" <> "->" <> "(" <> pretty res <> ")"
    Type'Concrete ty -> pretty ty

instance Pretty (SynType CompTc) where
  pretty = \case
    SynType'Var _ var -> pretty var
    SynType'ForAll _ vars ty -> "forall" <+> hsep (pretty <$> vars) <> "." <+> pretty ty
    SynType'Fun _ ty1 ty2 -> pretty ty1 <+> "->" <+> pretty ty2
    SynType'Concrete _ ty -> pretty ty

parensIndent :: Doc ann -> Doc ann
parensIndent x = parens (line <> indent 2 x <> line)

instance Pretty (SynTerm CompTc) where
  pretty = \case
    SynTerm'Var _ var -> pretty var
    SynTerm'Lit _ val -> pretty val
    SynTerm'App anno term1 term2 ->
      hsep
        [ parensIndent (parensIndent (pretty term1) <> line <> indent 2 (pretty term2))
        , "::"
        , pretty anno.annoType
        ]
    SynTerm'Lam anno var term ->
      hsep
        [ parensIndent
            ( "\\"
                <> pretty var
                <> "."
                <> line
                <> indent 2 (parensIndent (pretty term))
            )
        , "::"
        , pretty anno.annoType
        ]
    SynTerm'ALam anno var ty term ->
      hsep
        [ parens
            ( line
                <> "\\"
                <> parens
                  ( hsep
                      [ pretty var.varName
                      , "::"
                      , braces (pretty ty)
                      , "::"
                      , pretty var.varType
                      ]
                  )
                <> "."
                <+> pretty term
                <> line
            )
        , "::"
        , pretty anno.annoType
        ]
    SynTerm'Let anno var term1 term2 ->
      hsep
        [ parensIndent
            ( vsep
                [ "let"
                , indent
                    2
                    ( vsep
                        [ pretty var.varName <+> "="
                        , indent
                            2
                            ( hsep
                                [ parensIndent (pretty term1)
                                , "::"
                                , pretty var.varType
                                ]
                            )
                        ]
                    )
                , "in"
                , indent 2 (pretty term2)
                ]
            )
        , "::"
        , pretty anno.annoType
        ]
    SynTerm'Ann anno term ty ->
      hsep
        [ parensIndent
            ( hsep
                [ parensIndent (pretty term)
                , "::"
                , braces (pretty ty)
                ]
            )
        , "::"
        , pretty anno.annoType
        ]

-- =======================================
-- Instances for Zn (zonking stage) things
-- =======================================

instance Eq ZnTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord ZnTyVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Show ZnTyVar where
  show ZnTyVar{varName} = show varName

instance Pretty ZnTyVar where
  pretty ZnTyVar{varName} = pretty varName

instance Pretty ZnTermVar where
  pretty ZnTermVar{varName, varType} = parens (pretty varName <+> "::" <+> pretty varType)

instance Pretty (Type CompZn) where
  pretty = \case
    Type'Var var -> pretty var
    Type'ForAll vars body -> "forall" <+> hcat (pretty <$> vars) <> "." <+> pretty body
    Type'Fun arg res -> arg' <+> "->" <+> pretty res
     where
      arg' =
        pretty arg
          & case arg of
            Type'Fun _ _ -> parens
            Type'ForAll _ _ -> parens
            _ -> id
    Type'Concrete ty -> pretty ty

instance Pretty (SynType CompZn) where
  pretty = \case
    SynType'Var _ var -> pretty var
    SynType'ForAll _ vars ty -> "forall" <+> hsep (pretty <$> vars) <> "." <+> pretty ty
    SynType'Fun _ ty1 ty2 -> pretty ty1 <+> "->" <+> pretty ty2
    SynType'Concrete _ ty -> pretty ty

instance Pretty (SynTerm CompZn) where
  pretty = \case
    SynTerm'Var _ var -> pretty var
    SynTerm'Lit _ val -> pretty val
    SynTerm'App anno term1 term2 ->
      hsep
        [ parensIndent (parensIndent (pretty term1) <> line <> indent 2 (pretty term2))
        , "::"
        , pretty anno.annoType
        ]
    SynTerm'Lam anno var term ->
      hsep
        [ parensIndent
            ( "\\"
                <> pretty var
                <> "."
                <> line
                <> indent 2 (parensIndent (pretty term))
            )
        , "::"
        , pretty anno.annoType
        ]
    SynTerm'ALam anno var ty term ->
      hsep
        [ parens
            ( line
                <> "\\"
                <> parens
                  ( hsep
                      [ pretty var.varName
                      , "::"
                      , braces (pretty ty)
                      , "::"
                      , pretty var.varType
                      ]
                  )
                <> "."
                <+> pretty term
                <> line
            )
        , "::"
        , pretty anno.annoType
        ]
    SynTerm'Let anno var term1 term2 ->
      hsep
        [ parensIndent
            ( vsep
                [ "let"
                , indent
                    2
                    ( vsep
                        [ pretty var.varName <+> "="
                        , indent
                            2
                            ( hsep
                                [ parensIndent (pretty term1)
                                , "::"
                                , pretty var.varType
                                ]
                            )
                        ]
                    )
                , "in"
                , indent 2 (pretty term2)
                ]
            )
        , "::"
        , pretty anno.annoType
        ]
    SynTerm'Ann anno term ty ->
      hsep
        [ parensIndent
            ( hsep
                [ parensIndent (pretty term)
                , "::"
                , braces (pretty ty)
                ]
            )
        , "::"
        , pretty anno.annoType
        ]
