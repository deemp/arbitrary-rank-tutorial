module Language.Arralac.Typecheck.BasicTypes where

-- This module defines the basic types used by the type checker
-- Everything defined in here is exported

import Data.Data (Data (..))
import Data.IORef
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.LSP.Protocol.Types (UInt)
import Prettyprinter
import Prelude hiding ((<>))

-- ==============================================
-- [AST]
-- ==============================================

-- | Terms AST in the TTG representation.
--
-- Similar to 'HsExpr' in GHC.
--
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

-- TODO make each constructor a record?

-- TODO add parentheses?
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L382
-- https://github.com/ghc/ghc/blob/ef03d8b8851a1cace5f792fe5a91b6b227198aa2/compiler/Language/Haskell/Syntax/Expr.hs#L585

-- TODO add type extension field

-- TODO add add extension point to support Trees That Grow
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L537
-- TODO explain what it can be used for
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Expr.hs#L1294

-- | Type AST in TTG representation
--
-- Similar to 'HsType' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Type.hs#L812
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

-- | A literal.
--
-- Its constructors don't have extension points
-- because 'SynLit' is wrapped into 'SynType'Concrete' that does.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Lit.hs#L48
data SynLit
  = SynLit'Num Integer
  | SynLit'Bool Bool
  | SynLit'Str FastString
  | SynLit'Con FastString

-- ==============================================
-- [Type families for Term AST nodes]
-- ==============================================

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

type family XSynTerm'VarCommon x
type family XSynTerm'AnnoCommon x

type instance XSynTerm'VarCommon CompRn = Name
type instance XSynTerm'VarCommon CompTc = TcTermVar
type instance XSynTerm'VarCommon CompZn = ZnTermVar

type instance XSynTerm'AnnoCommon CompRn = SrcSpan
type instance XSynTerm'AnnoCommon CompTc = TcAnno
type instance XSynTerm'AnnoCommon CompZn = ZnAnno

-- In GHC, the extension field for the variable AST node constructor
-- is set to NoExtField.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Expr.hs#L238
--
-- We set it to () and use the extension field
-- in other constructors for annotations.

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

-- ==============================================
-- [Type families for Type AST nodes]
-- ==============================================

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

-- We use different representations for term and type variables.
--
-- In our calculus, term variables don't have types, while in GHC they do.
--
-- GHC uses the same 'LIdP p' type to represent type and term variables in the AST.
--
-- Term variables:
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L334
--
-- Type variables:
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Type.hs#L828

type instance XSynType'Var' x = ()
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

-- We record the type inside the 'Concrete',
-- not in the annotation.
type instance XSynType'Concrete' x = ()
type instance XSynType'Concrete x = Concrete

-- ==============================================
-- [Type family for variables]
-- ==============================================

type family XVar' p

type instance XVar' CompRn = RnVar
type instance XVar' CompTc = TcTyVar
type instance XVar' CompZn = ZnTyVar

-- ==============================================
-- [Compiler pass]
-- ==============================================
-- [We don't implement a compiler (yet),]
-- but still follow the naming used in GHC.

-- | Pass of the compiler.
--
-- Similar to 'CompPass in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L169
data Pass = Renamed | Typechecked | Zonked
  deriving stock (Data)

-- | Used as a data type index for the 'SynTerm', 'SynType', 'Type'.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L157
data CompPass (c :: Pass) where
  CompRn :: CompPass 'Renamed
  CompTc :: CompPass 'Typechecked
  CompZn :: CompPass 'Zonked

-- Type synonyms as a shorthand for tagging
--
-- Similar to 'GhcPs' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L173

-- | Output of renamer
type CompRn = CompPass 'Renamed

-- | Output of typechecker
type CompTc = CompPass 'Typechecked

-- | Output of zonker.
--
-- Doesn't contain metavariables by construction.
type CompZn = CompPass 'Zonked

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

-- Note [Ping-pong in TTG]
-- ~~~~~~~~~~~~~~~~~~~~~~
--
-- GHC wraps almost each node of the AST into a data constructor
-- that stores the node location in the source code.
--
-- Why should we annotate AST nodes without using the extension field of an AST node?
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

-- Currently, we do not follow the ping-pong approach.
-- Instead, we use 'Anno' in extension points of AST node constructors.

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L122
type family Anno a = b

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L647
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L108
type instance Anno Name = SrcSpan
type instance Anno SynLit = SrcSpan
type instance Anno TcTyVar = SrcSpan

-- TODO Use the ping-pong approach with the following definitions.

-- | Maps the "normal" id type for a given compiler pass.
--
-- Similar to 'IdGhcP' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L205
type family IdCompP pass where
  IdCompP 'Renamed = Name
  IdCompP 'Typechecked = TcTyVar
  -- GHC uses 'Id' here.
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L150
  IdCompP 'Zonked = ZnTyVar

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L759
data GenLocated l e = L l e
  deriving stock (Eq, Ord, Show, Data, Functor, Foldable, Traversable)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L764
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

-- ==============================================
-- [Names]
-- ==============================================

-- | A unique, unambiguous name for something, containing information about where that thing originated.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L126
data Name = Name
  { nameOcc :: OccName
  , nameUnique :: {-# UNPACK #-} !Unique
  , nameLoc :: !SrcSpan
  -- , nameSort :: NameSort
  -- See https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L148
  }
  deriving stock (Generic)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L144
data NameSpace
  = -- | Terms namespace.
    NameSpace'TermVar
  | -- | Types namespace.
    NameSpace'TypeVar
  | -- | Built-in types namespace.
    NameSpace'TypeConcrete
  deriving stock (Eq, Show)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/FastString.hs#L211
type FastString = Text

-- | Occurrence Name.
--
-- In this context that means:
--
-- "classified (i.e. as a type name, value name, etc)
-- but not qualified
-- and not yet resolved".
--
-- Similar to 'OccName' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L360
data OccName = OccName
  { occNameSpace :: !NameSpace
  , occNameFS :: !FastString
  }
  deriving stock (Show, Generic)

-- ==============================================
-- [Positions in the source code]
-- ==============================================

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L399
data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulGenerated
  | UnhelpfulOther !FastString
  deriving stock (Eq, Show)

-- TODO How does BNFC represent empty span?
-- Is its bound open on the right? Example : [start; finish)

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

-- ==============================================
-- [Variables]
-- ==============================================

-- TODO make a newtype

-- | Similar to 'Unique' in GHC
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique.hs#L98
type Unique = Int

-- | Similar to 'TcLevel' in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L698
--
-- Also see Note [TcLevel invariants] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L705
newtype TcLevel = TcLevel Int
  deriving newtype (Show, Eq, Ord, Num)

-- TODO might need TcLevel
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L418

data Expected a = Infer (IORef a) | Check a

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

-- TODO add another Indirect variant for types zonked during typechecking.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15552#note_159240
--
-- The issue was mentioned here:
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Zonk/Type.hs#L260

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
--
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

-- | Type of expressions parameterised over the compiler pass.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L124
--
-- For type safety, we need different representations of 'Type'
-- during different passes. E.g., 'Type' may not contain
-- mutable variables after zonking.
--
-- GHC authors also considered using different representations for different passes.
--
-- A proposal to move 'TcTyVar' from 'Var'.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/15552#note_159240
--
-- See Note [TyVars and TcTyVars during type checking] in GHC.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L558
--
-- We use different representations of type variables via the 'XVar'' type family.
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

data TypeConcrete
  = TypeConcrete'Int
  | TypeConcrete'Bool
  | TypeConcrete'String
  | TypeConcrete'Con FastString
  deriving stock (Eq)

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
-- [Implicit parameters]
-- ==============================================

-- | A global counter used for creating globally unique names.
--
-- GHC implements a much more sophisticated unique supply.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique/Supply.hs#L67
type IUniqueSupply = (?uniqueSupply :: IORef Int)

-- | Current file path.
type ICurrentFilePath = (?currentFilePath :: FastString)

-- | Whether to output debug logs.
type IDebug = (?debug :: Bool)

-- ==============================================
-- [Class for pretty printing]
-- ==============================================
-- Allows for configuring verbosity.

data PrettyVerbosity
  = PrettyVerbosity'Normal
  | PrettyVerbosity'Detailed
  | PrettyVerbosity'User

type IPrettyVerbosity = (?prettyVerbosity :: PrettyVerbosity)

class Pretty' a where
  pretty' :: (IPrettyVerbosity) => a -> Doc ann

prettyCompact :: (Pretty' a) => a -> Doc ann
prettyCompact = let ?prettyVerbosity = PrettyVerbosity'Normal in pretty'

prettyDetailed :: (Pretty' a) => a -> Doc ann
prettyDetailed = let ?prettyVerbosity = PrettyVerbosity'Detailed in pretty'

prettyUser :: (Pretty' a) => a -> Doc ann
prettyUser = let ?prettyVerbosity = PrettyVerbosity'User in pretty'

prettyIndent :: (IPrettyVerbosity, Pretty' a) => a -> Doc ann
prettyIndent = indent 2 . pretty'

vsep' :: [Doc ann] -> Doc ann
vsep' xs = encloseSep "" "" line xs <> line

instance {-# OVERLAPPABLE #-} (Pretty' a) => Show a where
  show =
    let ?prettyVerbosity = PrettyVerbosity'Detailed
     in show . pretty'

-- ==============================================
-- [Miscellaneous instances]
-- ==============================================

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

instance Pretty' SynLit where
  pretty' = \case
    SynLit'Num val -> pretty' val
    SynLit'Str val -> "\"" <> pretty' val <> "\""
    SynLit'Bool val -> pretty' val
    SynLit'Con val -> pretty' val

instance Pretty' RealSrcSpan where
  pretty' r =
    (pretty' r.srcSpanFile <> ":")
      <> (pretty' (r.srcSpanSLine + 1) <> ":" <> pretty' (r.srcSpanSCol + 1))
      <> "-"
      <> (pretty' (r.srcSpanELine + 1) <> ":" <> pretty' (r.srcSpanECol + 1))

instance Pretty' UnhelpfulSpanReason where
  pretty' = \case
    UnhelpfulNoLocationInfo -> "No location"
    UnhelpfulGenerated -> "Generated"
    UnhelpfulOther reason -> pretty' reason

instance Pretty' SrcSpan where
  pretty' = \case
    RealSrcSpan sp -> pretty' sp
    UnhelpfulSpan reason -> "Unknown span:" <+> pretty' reason

instance Pretty' SkolemInfoAnon where
  pretty' = \case
    SigSkol _ _ _ -> "SigSkol"
    SigTypeSkol _ -> "SigTypeSkol"
    ForAllSkol _ -> "ForAllSkol"
    InferSkol _ -> "InferSkol"
    UnifyForAllSkol _ -> "UnifyForAllSkol"

-- ==============================================
-- [Instances for 'Name']
-- ==============================================

instance Eq Name where
  n1 == n2 = n1.nameUnique == n2.nameUnique

instance Ord Name where
  n1 <= n2 = n1.nameUnique <= n2.nameUnique

instance Pretty' Name where
  pretty' name =
    case ?prettyVerbosity of
      PrettyVerbosity'Normal ->
        case name.nameOcc.occNameSpace of
          NameSpace'TypeConcrete -> pretty' name.nameOcc.occNameFS
          _ -> pretty' name.nameOcc.occNameFS <> "_" <> pretty' name.nameUnique
      PrettyVerbosity'Detailed ->
        pretty' name.nameOcc.occNameFS
          <> brackets ("ID" <+> pretty' name.nameUnique <> "," <+> pretty' name.nameLoc)
      PrettyVerbosity'User ->
        pretty' name.nameOcc.occNameFS

-- ==============================================
-- [Instances for Types]
-- ==============================================

typeConcreteName :: TypeConcrete -> FastString
typeConcreteName = \case
  TypeConcrete'Int -> "Int"
  TypeConcrete'Bool -> "Bool"
  TypeConcrete'String -> "String"
  TypeConcrete'Con name -> name

instance Pretty' TypeConcrete where
  pretty' = pretty' . typeConcreteName

instance Pretty' Concrete where
  pretty' c = pretty' c.concreteType

instance (Pretty' a) => Pretty' (Expected a) where
  pretty' (Infer _) = "[Infer]"
  pretty' (Check a) = "[Check]: " <> pretty' a

-- ==============================================
-- [Instances for CompRn types]
-- ==============================================

instance Eq RnVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord RnVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Pretty' RnVar where
  pretty' var = pretty' var.varName

instance Pretty' (SynType CompRn) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> parens (pretty' ty1) <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty

instance Pretty' (SynTerm CompRn) where
  pretty' = \case
    SynTerm'Var _ var -> pretty' var
    SynTerm'Lit _ val -> pretty' val
    SynTerm'App _ term1 term2 -> parens (pretty' term1) <+> pretty' term2
    SynTerm'Lam _ var term -> "\\" <> pretty' var <> "." <+> pretty' term
    SynTerm'ALam _ var ty term -> "\\" <> parens (pretty' var <+> "::" <+> pretty' ty) <> "." <+> pretty' term
    SynTerm'Let _ name term1 term2 -> "let" <+> pretty' name <+> "=" <+> pretty' term1 <+> "in" <+> pretty' term2
    SynTerm'Ann _ term ty -> parens (parens (pretty' term) <+> "::" <+> pretty' ty)

-- ==============================================
-- [Instances for CompTc types]
-- ==============================================

instance Eq TcTyVar where
  var1 == var2 = var1.varName == var2.varName

instance Ord TcTyVar where
  var1 <= var2 = var1.varName <= var2.varName

instance Pretty' TcLevel where
  pretty' (TcLevel lvl) = "L" <+> pretty' lvl

instance Pretty' Int where
  pretty' = pretty

instance Pretty' Integer where
  pretty' = pretty

instance Pretty' Bool where
  pretty' = pretty

instance {-# OVERLAPPABLE #-} (Pretty' a) => Pretty' [a] where
  pretty' xs =
    encloseSep
      "[ "
      (sp <> "]")
      (line <> comma <> space)
      (pretty' <$> xs)
   where
    sp =
      case xs of
        [] -> ""
        [_] -> " "
        _ -> line

instance Pretty' String where
  pretty' = pretty

instance Pretty' FastString where
  pretty' = pretty

instance (Pretty' a) => Pretty' (Maybe a) where
  pretty' = maybe "" pretty'

instance (Pretty' a, Pretty' b) => Pretty' (a, b) where
  pretty' (a, b) = parens (pretty' a <> "," <+> pretty' b)

instance Pretty' UInt where
  pretty' = pretty' . show

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

instance Pretty' (SynType CompTc) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> pretty' ty1 <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty

parensIndent :: Doc ann -> Doc ann
parensIndent x = parens (line <> indent 2 x <> line)

instance Pretty' (SynTerm CompTc) where
  pretty' = \case
    SynTerm'Var _ var -> pretty' var
    SynTerm'Lit _ val -> pretty' val
    SynTerm'App anno term1 term2 ->
      hsep
        [ parensIndent (parensIndent (pretty' term1) <> line <> indent 2 (pretty' term2))
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Lam anno var term ->
      hsep
        [ parensIndent
            ( "\\"
                <> pretty' var
                <> "."
                <> line
                <> indent 2 (parensIndent (pretty' term))
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'ALam anno var ty term ->
      hsep
        [ parens
            ( line
                <> "\\"
                <> parens
                  ( hsep
                      [ pretty' var.varName
                      , "::"
                      , braces (pretty' ty)
                      , "::"
                      , pretty' var.varType
                      ]
                  )
                <> "."
                <+> pretty' term
                <> line
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Let anno var term1 term2 ->
      hsep
        [ parensIndent
            ( vsep
                [ "let"
                , indent
                    2
                    ( vsep
                        [ pretty' var.varName <+> "="
                        , indent
                            2
                            ( hsep
                                [ parensIndent (pretty' term1)
                                , "::"
                                , pretty' var.varType
                                ]
                            )
                        ]
                    )
                , "in"
                , indent 2 (pretty' term2)
                ]
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Ann anno term ty ->
      hsep
        [ parensIndent
            ( hsep
                [ parensIndent (pretty' term)
                , "::"
                , braces (pretty' ty)
                ]
            )
        , "::"
        , pretty' anno.annoType
        ]

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

instance Pretty' (SynType CompZn) where
  pretty' = \case
    SynType'Var _ var -> pretty' var
    SynType'ForAll _ vars body -> "forall" <+> hsep (pretty' <$> vars) <> "." <+> pretty' body
    SynType'Fun _ ty1 ty2 -> pretty' ty1 <+> "->" <+> pretty' ty2
    SynType'Concrete _ ty -> pretty' ty

instance Pretty' (SynTerm CompZn) where
  pretty' = \case
    SynTerm'Var _ var -> pretty' var
    SynTerm'Lit _ val -> pretty' val
    SynTerm'App anno term1 term2 ->
      hsep
        [ parensIndent (parensIndent (pretty' term1) <> line <> indent 2 (pretty' term2))
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Lam anno var term ->
      hsep
        [ parensIndent
            ( "\\"
                <> pretty' var
                <> "."
                <> line
                <> indent 2 (parensIndent (pretty' term))
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'ALam anno var ty term ->
      hsep
        [ parens
            ( line
                <> "\\"
                <> parens
                  ( hsep
                      [ pretty' var.varName
                      , "::"
                      , braces (pretty' ty)
                      , "::"
                      , pretty' var.varType
                      ]
                  )
                <> "."
                <+> pretty' term
                <> line
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Let anno var term1 term2 ->
      hsep
        [ parensIndent
            ( vsep
                [ "let"
                , indent
                    2
                    ( vsep
                        [ pretty' var.varName <+> "="
                        , indent
                            2
                            ( hsep
                                [ parensIndent (pretty' term1)
                                , "::"
                                , pretty' var.varType
                                ]
                            )
                        ]
                    )
                , "in"
                , indent 2 (pretty' term2)
                ]
            )
        , "::"
        , pretty' anno.annoType
        ]
    SynTerm'Ann anno term ty ->
      hsep
        [ parensIndent
            ( hsep
                [ parensIndent (pretty' term)
                , "::"
                , braces (pretty' ty)
                ]
            )
        , "::"
        , pretty' anno.annoType
        ]
