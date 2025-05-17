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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.STLC.Typing.Jones2007.BasicTypes where

-- This module defines the basic types used by the type checker
-- Everything defined in here is exported

import Control.Monad (forM)
import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data (..))
import Data.IORef
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Language.STLC.Common ()
import Language.STLC.Syntax.Abs qualified as Abs
import Prettyprinter
import Prelude hiding ((<>))

-- infixr 4 --> -- The arrow type constructor
-- infixl 4 `App` -- Application

-----------------------------------
--      Ubiquitous types        --
-----------------------------------

-- data Name ann = Name ann Text
--   deriving (Functor) -- Names are very simple

-- instance Eq (Name ann) where
--   Name _ x == Name _ y = x == y

-- instance Ord (Name ann) where
--   Name _ x <= Name _ y = x <= y

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
  | -- | Parentheses
    --
    -- @(a -> b)@
    SynType'Paren (XSynType'Paren' x) (XSynType'Paren x)
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

type family XSynType'Paren' x
type family XSynType'Paren x

type family XSynType'Concrete' x
type family XSynType'Concrete x

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L169
data Pass = Renamed | Typechecked | Zonked
  deriving (Data)

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
  deriving (Eq, Ord, Show, Data, Functor, Foldable, Traversable)

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
  deriving (Eq, Ord, Show, Data, Functor, Foldable, Traversable)

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
  deriving (Eq, Show)

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
  deriving (Show)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L399
data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulGenerated
  | UnhelpfulOther !FastString
  deriving (Eq, Show)

-- | Real Source Span
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L367
data RealSrcSpan
  = RealSrcSpan'
  { srcSpanFile :: !FastString
  , srcSpanSLine :: {-# UNPACK #-} !Int
  , srcSpanSCol :: {-# UNPACK #-} !Int
  , srcSpanELine :: {-# UNPACK #-} !Int
  , srcSpanECol :: {-# UNPACK #-} !Int
  }
  deriving (Eq)

-- | Source Span
--
-- A 'SrcSpan' identifies either a specific portion of a text file
-- or a human-readable description of a location.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L392
data SrcSpan
  = -- TODO Replace with RealSrcSpan
    RealSrcSpan !Abs.BNFC'Position
  | UnhelpfulSpan UnhelpfulSpanReason
  deriving (Show)

-- | The key type representing kinds in the compiler.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L110
type Kind = Type

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L107
type KindOrType = Type

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L163
type TyVar = RnVar

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
instance Show (Type p)

-- TODO implement
instance Pretty (Type p)

instance Pretty (TypeConcrete)

-- TODO separate Type from TcType
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L576
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Zonk/Type.hs#L260
-- Possible design: two types of Indirects
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15552#note_158972
-- + Separate TcType from Type
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15552#note_159240

-- TODO use CompZn

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

-- TODO implement
instance Show SkolemInfoAnon

-- TODO implement
instance Pretty SkolemInfoAnon

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
  = Flexi -- Flexi type variables unify to become Indirects
  -- Means that the type variable is unfilled
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L915
  | Indirect TcType

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L698
data TcLevel = TcLevel {-# UNPACK #-} !Int

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
  = -- | Deep skolemisation doesn't affect argument sigmas, only result ones (p. 24).
    -- We don't have deep instantiation (p. 28).
    -- The original definition of TyVar had a BoundTv constructor (p.41).
    -- Hence, we should be able to represent bound tyvars during typechecking.
    BoundTv
      { tcLevel :: TcLevel
      }
  | -- A skolem
    SkolemTv
      { skolemInfo :: SkolemInfo
      -- ^ See Note [Keeping SkolemInfo inside a SkolemTv]
      , tcLevel :: TcLevel
      -- ^ Level of the implication that binds it
      -- See GHC.Tc.Utils.Unify Note [Deeper level on the left] for
      --     how this level number is used
      }
  | MetaTv
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
  deriving (Show)

instance Eq Name where
  n1 == n2 = n1.nameUnique == n2.nameUnique

instance Ord Name where
  n1 <= n2 = n1.nameUnique <= n2.nameUnique

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

-- instance (HasField "varName" b Name) => Eq b where
--   var1 == var2 = var1.varName == var2.varName

instance Eq RnVar where
  var1 == var2 = var1.varName == var2.varName

instance Eq TcTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Eq ZnTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord RnVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Ord TcTyVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Ord ZnTyVar where
  var1 <= var2 = var1.varName <= var2.varName

-- TODO implement
instance Show RnVar
instance Show TcTyVar
instance Show ZnTyVar

-- TODO implement
instance Pretty RnVar
instance Pretty TcTyVar
instance Pretty ZnTyVar

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Module/Name.hs#L13
newtype ModuleName = ModuleName Text deriving (Show, Eq)

-- TODO specify instances for each phase
-- TODO store instances in a separate module

---------------- Terms ---------------------

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

type instance XSynTerm'Lit' CompRn = SrcSpan
type instance XSynTerm'Lit' CompTc = TypeConcrete
type instance XSynTerm'Lit' CompZn = TypeConcrete
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

type instance XSynType'Paren' x = SrcSpan
type instance XSynType'Paren x = SynType x

-- TODO explain when to use annotations and when not to
type instance XSynType'Concrete' x = ()
type instance XSynType'Concrete CompRn = Name
type instance XSynType'Concrete CompTc = Concrete
type instance XSynType'Concrete CompZn = Concrete

type NameFs = FastString

type IUniqueSupply = (?uniqueSupply :: IORef Int)
type IScope = (?scope :: Map NameFs Int)

-- TODO add index because the parse type isn't enough to differentiate
-- TODO add built-in types to the scope
-- TODO Should we have a separate scope for terms and types?
type IConvertRename = (IUniqueSupply, IScope)

newUnique :: (IUniqueSupply) => IO Int
newUnique = do
  r <- readIORef ?uniqueSupply
  writeIORef ?uniqueSupply (r + 1)
  pure r

getEnvVarId :: (IScope) => NameFs -> Maybe Int
getEnvVarId k = Map.lookup k ?scope

getUnique :: (IConvertRename) => NameFs -> IO Int
getUnique name = maybe newUnique pure (getEnvVarId name)

withNameInScope :: (IConvertRename) => Name -> ((IConvertRename) => IO a) -> IO a
withNameInScope name act =
  let ?scope = Map.insert name.nameOcc.occNameFS name.nameUnique ?scope
   in act

withNamesInScope :: (IConvertRename) => [Name] -> ((IConvertRename) => IO a) -> IO a
withNamesInScope names act =
  -- Map.union prefers the first argument
  -- and we need to add new names to the scope
  -- to implement shadowing
  let ?scope = Map.union (Map.fromList ((\name -> (name.nameOcc.occNameFS, name.nameUnique)) <$> names)) ?scope
   in act

-- TODO use different annotations in different phases

class ConvertAbsToBT a where
  type To a
  convertAbsToBT :: (IConvertRename) => a -> (To a)

instance ConvertAbsToBT Abs.Exp where
  type To Abs.Exp = IO (SynTerm CompRn)
  convertAbsToBT :: (IConvertRename) => Abs.Exp -> To Abs.Exp
  convertAbsToBT = \case
    Abs.ExpVar _pos var -> do
      SynTerm'Var ()
        <$> convertAbsToBT var False
    Abs.ExpInt pos val ->
      pure $ SynTerm'Lit (RealSrcSpan pos) (SynLit'Num val)
    Abs.ExpApp pos term1 term2 ->
      SynTerm'App (RealSrcSpan pos)
        <$> (convertAbsToBT term1)
        <*> (convertAbsToBT term2)
    Abs.ExpAbs pos var term -> do
      var' <- convertAbsToBT var True
      term' <- withNameInScope var' (convertAbsToBT term)
      pure $ SynTerm'Lam (RealSrcSpan pos) var' term'
    Abs.ExpAbsAnno pos var ty term -> do
      var' <- convertAbsToBT var True
      ty' <- convertAbsToBT ty
      term' <- withNameInScope var' (convertAbsToBT term)
      pure $ SynTerm'ALam (RealSrcSpan pos) var' ty' term'
    Abs.ExpLet pos var term1 term2 -> do
      var' <- convertAbsToBT var True
      -- TODO should the let-expression be recursive
      -- and the var be brought into scope of the assigned term?
      term1' <- convertAbsToBT term1
      term2' <- withNameInScope var' (convertAbsToBT term2)
      pure $ SynTerm'Let (RealSrcSpan pos) var' term1' term2'
    Abs.ExpAnno pos term ty ->
      SynTerm'Ann (RealSrcSpan pos)
        <$> (convertAbsToBT term)
        <*> (convertAbsToBT ty)

-- TODO create unique only for new binders
-- TODO what to do with free variables? Report error?

instance ConvertAbsToBT Abs.Var where
  type To Abs.Var = Bool -> IO Name
  convertAbsToBT (Abs.Var pos (Abs.NameLowerCase name)) needUnique = do
    nameUnique <- if needUnique then newUnique else getUnique name
    pure $
      Name
        { nameOcc =
            OccName
              { occNameSpace = NameSpace'Term
              , occNameFS = name
              }
        , nameUnique
        , nameLoc = RealSrcSpan pos
        }

instance ConvertAbsToBT Abs.TypeVariable where
  type To Abs.TypeVariable = IO Name
  convertAbsToBT (Abs.TypeVariableName pos (Abs.NameLowerCase name)) = do
    nameUnique <- newUnique
    pure $
      Name
        { nameOcc =
            OccName
              { occNameSpace = NameSpace'Type'Var
              , occNameFS = name
              }
        , nameUnique
        , nameLoc = RealSrcSpan pos
        }

instance ConvertAbsToBT Abs.Type where
  type To Abs.Type = IO (SynType CompRn)
  convertAbsToBT = \case
    -- TODO not a variable
    Abs.TypeConcrete pos (Abs.NameUpperCase name) -> do
      -- TODO should all mentions of a type have the same uniques?
      nameUnique <- getUnique name
      pure $
        SynType'Concrete
          ()
          Name
            { nameOcc =
                OccName
                  { occNameSpace = NameSpace'Type'Concrete
                  , occNameFS = name
                  }
            , nameUnique
            , nameLoc = RealSrcSpan pos
            }
    Abs.TypeVariable pos (Abs.NameLowerCase name) -> do
      nameUnique <- getUnique name
      pure $
        SynType'Var
          ()
          ( Name
              { nameOcc =
                  OccName
                    { occNameSpace = NameSpace'Type'Var
                    , occNameFS = name
                    }
              , nameUnique
              , nameLoc = RealSrcSpan pos
              }
          )
    Abs.TypeForall pos tys ty -> do
      tys' <- forM tys (\x -> convertAbsToBT x)
      ty' <- withNamesInScope tys' (convertAbsToBT ty)
      pure $ SynType'ForAll (RealSrcSpan pos) tys' ty'
    Abs.TypeFunc pos ty1 ty2 -> SynType'Fun (RealSrcSpan pos) <$> (convertAbsToBT ty1) <*> (convertAbsToBT ty2)
    Abs.TypeParen pos ty -> SynType'Paren (RealSrcSpan pos) <$> convertAbsToBT ty

instance Pretty SynLit where
  pretty = \case
    SynLit'Num val -> pretty val
    SynLit'Str val -> pretty val
    SynLit'Bool val -> pretty val

instance Pretty (SynTerm CompRn) where
  pretty = \case
    SynTerm'Var _ var -> pretty var
    SynTerm'Lit _ val -> pretty val
    SynTerm'App _ term1 term2 -> parens (pretty term1) <+> pretty term2
    SynTerm'Lam _ var term -> "\\" <> pretty var <> "." <+> pretty term
    SynTerm'ALam _ var ty term -> "\\" <> pretty var <+> "::" <+> pretty ty <> "." <+> pretty term
    SynTerm'Let _ name term1 term2 -> "let" <+> pretty name <+> "=" <+> pretty term1 <+> "in" <+> pretty term2
    SynTerm'Ann _ term ty -> parens (pretty term <+> "::" <+> pretty ty)

instance Pretty Name where
  pretty name =
    case name.nameOcc.occNameSpace of
      NameSpace'Type'Concrete -> pretty name.nameOcc.occNameFS
      _ -> pretty name.nameOcc.occNameFS <> "_" <> pretty name.nameUnique

instance Pretty (SynType CompRn) where
  pretty = \case
    SynType'Var _ var -> pretty var
    SynType'ForAll _ vars ty -> "forall" <+> hsep (pretty <$> vars) <> "." <+> pretty ty
    SynType'Fun _ ty1 ty2 -> pretty ty1 <+> "->" <+> pretty ty2
    SynType'Paren _ ty -> parens (pretty ty)
    SynType'Concrete _ ty -> pretty ty

ex1 :: Abs.Exp
ex1 = "\\ (x :: forall b. Int). (\\ z. z) x"

ex2 :: IO (Doc ann)
ex2 = do
  let builtInTypes = ["Int"]
  uniqueSupply <- newIORef (length builtInTypes)
  let
    ?scope = Map.fromList (zip builtInTypes [0 ..])
    ?uniqueSupply = uniqueSupply
  pretty <$> convertAbsToBT ex1

-- >>> ex2
-- \x_1 :: forall b_2. Int. (\z_3. z_3) x_1

-- -----------------------------------
-- --      Types                   --
-- -----------------------------------

type Sigma = TcType
type Rho = TcType -- No top-level ForAll
type Tau = TcType -- No ForAlls anywhere

data TypeConcrete
  = TypeConcrete'Int
  | TypeConcrete'Bool
  | TypeConcrete'String
  deriving (Eq)

instance Show TypeConcrete where
  show = \case
    TypeConcrete'Int -> "Int"
    TypeConcrete'Bool -> "Bool"
    TypeConcrete'String -> "String"

-- ---------------------------------
-- --      Constructors

(-->) :: Sigma -> Sigma -> Sigma
arg --> res = Type'Fun arg res

-- ---------------------------------
-- --  Free and bound variables

-- Get the MetaTvs from a type; no duplicates in result
metaTvs :: [TcType] -> [TcTyVar]
metaTvs tys = nubOrd (foldr (flip go) [] tys)
 where
  go :: [TcTyVar] -> TcType -> [TcTyVar]
  go acc = \case
    Type'Var var ->
      case var of
        TcTyVar{varDetails = MetaTv{}} -> var : acc
        _ -> acc
    Type'ForAll _ ty -> go acc ty
    Type'Concrete _ -> acc
    Type'Fun ty1 ty2 -> go (go acc ty1) ty2

freeTyVars :: [TcType] -> [TcTyVar]
-- Get the free TyVars from a type; no duplicates in result
freeTyVars tys = nubOrd (foldr (go Set.empty) [] tys)
 where
  go ::
    -- Bound type variables
    Set.Set TcTyVar ->
    -- Type to look at
    TcType ->
    -- Accumulates result
    [TcTyVar] ->
    [TcTyVar]
  go bound v acc =
    case v of
      Type'Var var
        -- Ignore occurrences of bound type variables
        | Set.member var bound -> acc
        | otherwise -> var : acc
      Type'Concrete _ -> acc
      Type'ForAll vars ty -> go (Set.fromList vars <> bound) ty acc
      Type'Fun ty1 ty2 -> go bound ty1 (go bound ty2 acc)

tyVarBndrs :: Rho -> [TcTyVar]
-- Get all the binders used in ForAlls in the type, so that
-- when quantifying an outer for-all we can avoid these inner ones
tyVarBndrs ty = nub (bndrs ty)
 where
  bndrs = \case
    Type'ForAll vars body -> vars <> bndrs body
    -- (ForAll' tvs body) = tvs ++ bndrs body
    Type'Fun arg res -> bndrs arg <> bndrs res
    _ -> []

-- TODO return Name?
tyVarName :: TyVar -> Text
tyVarName = (.varName.nameOcc.occNameFS)

-- \| CycleBreakerTv -- Used to fix occurs-check problems in Givens
-- See Note [Type equality cycles] in
-- GHC.Tc.Solver.Equality
--  | ConcreteTv ConcreteTvOrigin
--       -- ^ A unification variable that can only be unified
--       -- with a concrete type, in the sense of
--       -- Note [Concrete types] in GHC.Tc.Utils.Concrete.
--       -- See Note [ConcreteTv] in GHC.Tc.Utils.Concrete.
--       -- See also Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete
--       -- for an overview of how this works in context.

-- -----------------------------------
-- --      Pretty printing class   --
-- -----------------------------------

-- class Outputable a where
--   ppr :: a (Maybe ann) -> Doc ann

-- docToString :: Doc ann -> String
-- -- TODO change
-- docToString = show

-- dcolon, dot :: Doc ann
-- dcolon = pretty "::"
-- dot = pretty '.'

-- -------------- Pretty-printing terms ---------------------

-- instance Pretty (Name ann) where
--   pretty (Name _ann n) = pretty n

-- instance Outputable Term where
--   ppr = \case
--     Var _ann n -> pprName n
--     Lit _ann i -> pretty i
--     App ann e1 e2 -> pprApp (App ann e1 e2)
--     Lam _ann v e -> sep [pretty '\\' <> pprName v <> pretty ".", ppr e]
--     ALam _ann v t e ->
--       sep
--         [ pretty '\\'
--             <> parens (pprName v <> dcolon <> ppr t)
--             <> pretty "."
--         , ppr e
--         ]
--     Let _ann v rhs b ->
--       sep
--         [ pretty "let {"
--         , nest 2 (pprName v <+> equals <+> ppr rhs <+> pretty '}')
--         , pretty "in"
--         , ppr b
--         ]
--     Ann _ann e ty -> pprParendTerm e <+> dcolon <+> pprParendType ty

-- instance Show (Term (Maybe ann)) where
--   show t = docToString (ppr t)

-- pprParendTerm :: Term (Maybe a) -> Doc a
-- pprParendTerm e
--   | atomicTerm e = ppr e
--   | otherwise = parens (ppr e)

-- pprApp :: Term (Maybe a) -> Doc a
-- pprApp e = go e []
--  where
--   go (App' e1 e2) es = go e1 (e2 : es)
--   go e' es = pprParendTerm e' <+> sep (map pprParendTerm es)

-- pprName :: Name -> Doc ann
-- pprName n = pretty n

-- -------------- Pretty-printing types ---------------------

-- instance Outputable Type where
--   ppr ty = pprType topPrec ty

-- instance Outputable MetaTv where
--   ppr (Meta _ann u _) = pretty "$" <> pretty u

-- instance Outputable TyVar where
--   ppr (BoundTv' n) = pretty n
--   ppr (SkolemTv' n u) = pretty n <+> pretty u

-- instance Show (Type (Maybe ann)) where
--   show t = docToString (ppr t)

-- type Precedence = Int
-- topPrec, arrPrec, tcPrec, atomicPrec :: Precedence
-- topPrec = 0 -- Top-level precedence
-- arrPrec = 1 -- Precedence of (a->b)
-- tcPrec = 2 -- Precedence of (T a b)
-- atomicPrec = 3 -- Precedence of t

-- precType :: (Type ann) -> Precedence
-- precType (ForAll' _ _) = topPrec
-- precType (Fun' _ _) = arrPrec
-- precType _ = atomicPrec

-- -- All the types are be atomic

-- pprParendType :: Type (Maybe ann) -> Doc ann
-- pprParendType ty = pprType tcPrec ty

-- pprType :: Precedence -> Type (Maybe ann) -> Doc ann
-- -- Print with parens if precedence arg > precedence of Type ann itself
-- pprType p ty
--   | p >= precType ty = parens (ppr_type ty)
--   | otherwise = ppr_type ty

-- ppr_type :: Type (Maybe ann) -> Doc ann -- No parens
-- ppr_type (ForAll' ns ty) =
--   sep
--     [ pretty "forall"
--         <+> hsep (map ppr ns)
--         <> dot
--     , ppr ty
--     ]
-- ppr_type (Fun' arg res) =
--   sep
--     [ pprType arrPrec arg <+> pretty "->"
--     , pprType (arrPrec - 1) res
--     ]
-- ppr_type (TyCon' tc) = ppr_tc tc
-- ppr_type (TyVar' n) = ppr n

-- -- ppr_type (MetaTv' tv) = ppr tv

-- ppr_tc :: TyCon -> Doc ann
-- ppr_tc IntT = pretty "Int"
-- ppr_tc BoolT = pretty "Bool"
