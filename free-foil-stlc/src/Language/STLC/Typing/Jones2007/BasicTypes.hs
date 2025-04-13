{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Language.STLC.Typing.Jones2007.BasicTypes where

-- This module defines the basic types used by the Type ann checker
-- Everything defined in here is exported

import Data.Data (Data (..), Typeable)
import Data.Data qualified as Data
import Data.IORef
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import Prettyprinter hiding (dot)
import Prelude hiding ((<>))

-- infixr 4 --> -- The arrow Type ann constructor
infixl 4 `App` -- Application

-----------------------------------
--      Ubiquitous types        --
-----------------------------------

-- data Name ann = Name ann Text
--   deriving (Functor) -- Names are very simple

-- instance Eq (Name ann) where
--   Name _ x == Name _ y = x == y

-- instance Ord (Name ann) where
--   Name _ x <= Name _ y = x <= y

-- TODO use a more advanced AST
-- https://github.com/ghc/ghc/blob/27029e60b17aa320d52a5bf31eef96506335b55f/compiler/GHC/Types/SrcLoc.hs#L758

-----------------------------------
--      Expressions             --
-----------------------------------
-- Examples below
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Expr.hs#L332
data Term x
  = -- | x
    Var (XVar x)
  | -- | 3
    Lit (XLit x)
  | -- | f x
    App (XAppFun x) (XAppArg x)
  | -- | \ x -> x
    Lam (XLamVar x) (XLamBody x)
  | -- | \ (x :: a) -> x
    ALam (XALamVar x) (XALamSigma x) (XALamTerm x)
  | -- | let x = f y in x+1
    Let (XLetName x) (XLetAssignedTerm x) (XInTerm x)
  | -- | (f x) :: Int
    Ann (XAnnTerm x) (XAnnSigma x)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L412
type family XLit x
type family XAppFun x
type family XAppArg x
type family XLamVar x
type family XLamBody x
type family XALamVar x
type family XALamSigma x
type family XALamTerm x
type family XLetName x
type family XLetAssignedTerm x
type family XInTerm x
type family XAnnTerm x
type family XAnnSigma x

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L169
data Pass = Parsed | Renamed | Typechecked
  deriving (Data)

-- | Used as a data type index for the hsSyn AST; also serves
-- as a singleton type for Pass
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L157
data CompPass (c :: Pass) where
  CompPs :: CompPass 'Parsed
  CompRn :: CompPass 'Renamed
  CompTc :: CompPass 'Typechecked

-- Type synonyms as a shorthand for tagging
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L173
type CompPs = CompPass 'Parsed -- Output of parser
type CompRn = CompPass 'Renamed -- Output of renamer
type CompTc = CompPass 'Typechecked -- Output of typechecker

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L120
type family XRec p a = r | r -> a

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L167
type family IdP p

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L169
type XVar p = XRec p (IdP p)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L202
type instance IdP (CompPass p) = IdCompP p

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L144
data NameSpace
  = -- | Variable name space (including "real" data constructors).
    VarName
  | -- | Type variable namespace.
    TvName
  deriving (Eq)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/FastString.hs#L211
type FastString = Text

-- | Occurrence Name
--
-- In this context that means:
-- "classified (i.e. as a type name, value name, etc) but not qualified
-- and not yet resolved"
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L360
data OccName = OccName
  { occurenceNameSpace :: !NameSpace
  , occurenceNameFS :: !FastString
  }

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Reader.hs#L166
data RdrName
  = -- | Unqualified  name
    --
    -- Used for ordinary, unqualified occurrences, e.g. @x@, @y@ or @Foo@.
    -- Create such a 'RdrName' with 'mkRdrUnqual'
    Unqual OccName
  | -- | Qualified name
    --
    -- A qualified name written by the user in
    -- /source/ code.  The module isn't necessarily
    -- the module where the thing is defined;
    -- just the one from which it is imported.
    -- Examples are @Bar.x@, @Bar.y@ or @Bar.Foo@.
    -- Create such a 'RdrName' with 'mkRdrQual'
    Qual ModuleName OccName

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L399
data UnhelpfulSpanReason = UnhelpfulNoLocationInfo deriving (Eq, Show)

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
  = RealSrcSpan !RealSrcSpan
  | UnhelpfulSpan !UnhelpfulSpanReason

-- | The key type representing kinds in the compiler.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L110
type Kind = Type

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L107
type KindOrType = Type

-- NOTE:  Other parts of the code assume that type literals do not contain
-- types or type variables.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L200
data TyLit
  = NumTyLit Integer
  | StrTyLit FastString
  | CharTyLit Char
  deriving (Eq, Data.Data)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L163
type TyVar = Var

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/TyCo/Rep.hs#L124
data Type
  = -- | Vanilla type variable
    TyVarTy Var
  | -- | Type application to something other than a 'TyCon'. Parameters:
    --
    --  1) Function: must /not/ be a 'TyConApp' or 'CastTy',
    --     must be another 'AppTy', or 'TyVarTy'
    --     See Note [Respecting definitional equality] \(EQ1) about the
    --     no 'CastTy' requirement
    --
    --  2) Argument type
    AppTy
      Type
      Type
  | ForAllTy
      [TyVar]
      Type
  | -- | Type literals are similar to type constructors.
    LitTy TyLit

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L346
type TcType = Type

-- A TyVarDetails is inside a TyVar
-- See Note [TyVars and TcTyVars during type checking]
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L601
data TcTyVarDetails
  = SkolemTv -- A skolem
      Unique
  | MetaTv (IORef TcType)

-- | Identifier
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L150
type Id = Var

-- | Unique identifier.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique.hs#L98
newtype Unique = MkUnique Word64

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L126
data Name = Name
  { nameOcc :: OccName
  , nameUnique :: {-# UNPACK #-} !Unique
  , nameLoc :: !SrcSpan
  }

-- | Variable
--
-- Essentially a typed 'Name', that may also contain some additional information
-- about the 'Var' and its use sites.
--
-- Variable is always local
--
-- FIXME If TyVar occurs during constraint solving, it means BoundTv
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L257
data Var
  = -- | Type and kind variables
    TyVar
      { varName :: !Name
      , varType :: Kind
      -- ^ The type or kind of the 'Var' in question
      }
  | -- | Used only during type inference
    TcTyVar
      { varName :: !Name
      , varType :: Kind
      , tcTyVarDetails :: TcTyVarDetails
      }
  | -- | Variable identifier
    -- Always local and vanilla.
    Id
      { varName :: !Name
      , varType :: Type
      }

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Module/Name.hs#L13
newtype ModuleName = ModuleName Text deriving (Show, Eq)

-- | Maps the "normal" id type for a given GHC pass
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L205
type family IdCompP pass where
  IdCompP 'Parsed = RdrName
  IdCompP 'Renamed = Name
  IdCompP 'Typechecked = Id

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L210
type LIdCompP p = XRecComp (IdCompP p)

-- | We attach SrcSpans to lots of things, so let's have a datatype for it.
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L759
data GenLocated l e = L l e
  deriving (Eq, Ord, Show, Data, Functor, Foldable, Traversable)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/Language/Haskell/Syntax/Extension.hs#L122
type family Anno a = b

-- (XRecComp tree) wraps `tree` in a Compiler-specific,
-- but pass-independent, source location
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Hs/Extension.hs#L105
type XRecComp a = GenLocated (Anno a) a

-- type Term' ann = Term (IORef (Maybe (Type ann)))

-- pattern Var' :: Name ann -> Term a
-- pattern Var' n <- Var _a n
-- pattern Lit' :: Integer -> Term a
-- pattern Lit' t <- Lit _a t
-- pattern App' :: Term a -> Term a -> Term a
-- pattern App' t1 t2 <- App _a t1 t2
-- pattern Lam' :: Name ann -> Term a -> Term a
-- pattern Lam' n t <- Lam _a n t
-- pattern ALam' :: Name ann -> Sigma ann -> Term a -> Term a
-- pattern ALam' n s t <- ALam _a n s t
-- pattern Let' :: Name ann -> Term a -> Term a -> Term a
-- pattern Let' n t1 t2 <- Let _a n t1 t2
-- pattern Ann' :: Term a -> Sigma ann -> Term a
-- pattern Ann' t s <- Ann _a t s

-- {-# COMPLETE Var', Lit', App', Lam', ALam', Let', Ann' #-}

-- atomicTerm :: Term a -> Bool
-- atomicTerm (Var' _) = True
-- atomicTerm (Lit' _) = True
-- atomicTerm _ = False

-- -----------------------------------
-- --      Types                   --
-- -----------------------------------

-- type Sigma ann = Type ann
-- type Rho ann = Type ann -- No top-level ForAll
-- type Tau ann = Type ann -- No ForAlls anywhere

-- data X ann x = X ann x

-- -- data Type ann
-- --   = ForAll [(TyVar ann)] (Rho ann) -- Forall type
-- --   | Fun (Type ann) (Type ann) -- Function type
-- --   | TyCon TyCon -- Type constants
-- --   | TyVar (TyVar ann) -- Always bound by a ForAll

-- -- data MetaTv = MetaTv ann (MetaTv ann) -- A meta Type ann variable
-- -- pattern MetaTv' :: MetaTv ann -> Type ann
-- -- pattern MetaTv' tv <- MetaTv _ann tv

-- -- pattern TyVar' :: TyVar ann -> Type ann
-- -- pattern TyVar' tv <- TyVar _ann tv
-- -- pattern TyCon' :: TyCon -> Type ann
-- -- pattern TyCon' tc <- TyCon _ann tc
-- -- pattern Fun' :: Type ann -> Type ann -> Type ann
-- -- pattern Fun' arg res <- Fun _ann arg res
-- -- pattern ForAll' :: [TyVar ann] -> Rho ann -> Type ann
-- -- pattern ForAll' tvs ty <- ForAll _ann tvs ty

-- -- {-# COMPLETE ForAll', Fun', TyCon', TyVar' #-}

-- -- data TyVar
-- --   = BoundTv Text -- A Type ann variable bound by a ForAll
-- --   | SkolemTv Text Uniq -- A skolem constant; the String is
-- --   -- just to improve error messages

-- -- pattern BoundTv' :: Text -> TyVar ann
-- -- pattern BoundTv' t <- BoundTv _ann t

-- -- pattern SkolemTv' :: Text -> Uniq -> TyVar ann
-- -- pattern SkolemTv' t u <- SkolemTv _ann t u

-- -- {-# COMPLETE BoundTv', SkolemTv' #-}

-- -- data MetaTv ann = Meta ann Uniq (TyRef ann) -- Can unify with any tau-type

-- -- data TyRef ann = TyRef ann (IORef (Maybe (Tau ())))

-- -- 'Nothing' means the Type ann variable is not substituted
-- -- 'Just ty' means it has been substituted by 'ty'

-- -- instance Eq (MetaTv ann) where
-- --   (Meta _ann1 u1 _) == (Meta _ann2 u2 _) = u1 == u2

-- -- instance Eq (TyVar ann) where
-- --   (BoundTv' s1) == (BoundTv' s2) = s1 == s2
-- --   (SkolemTv' _ u1) == (SkolemTv' _ u2) = u1 == u2
-- --   _ == _ = error "Mismatched Type ann variables"

-- type Uniq = Int

-- data TyCon = IntT | BoolT
--   deriving (Eq)

-- ---------------------------------
-- --      Constructors

-- (-->) :: Sigma (Maybe ann) -> Sigma (Maybe ann) -> Sigma (Maybe ann)
-- arg --> res = Fun Nothing arg res

-- intType, boolType :: Tau (Maybe ann)
-- intType = TyCon Nothing IntT
-- boolType = TyCon Nothing BoolT

-- ---------------------------------
-- --  Free and bound variables

-- metaTvs :: [Type ann] -> [MetaTv ann]
-- -- Get the MetaTvs from a type; no duplicates in result
-- metaTvs tys = foldr go [] tys
--  where
--   -- go (MetaTv' tv) acc
--   --   | tv `elem` acc = acc
--   --   | otherwise = tv : acc
--   go (TyVar' _) acc = acc
--   go (TyCon' _) acc = acc
--   go (Fun' arg res) acc = go arg (go res acc)
--   go (ForAll' _ ty) acc = go ty acc -- ForAll binds TyVars only

-- freeTyVars :: [Type ann] -> [TyVar ann]
-- -- Get the free TyVars from a type; no duplicates in result
-- freeTyVars tys = foldr (go []) [] tys
--  where
--   go ::
--     [TyVar ann] -> -- Ignore occurrences of bound Type ann variables
--     Type ann -> -- Type ann to look at
--     [TyVar ann] -> -- Accumulates result
--     [TyVar ann]
--   go bound (TyVar' tv) acc
--     | tv `elem` bound = acc
--     | tv `elem` acc = acc
--     | otherwise = tv : acc
--   -- go _bound (MetaTv' _) acc = acc
--   go _bound (TyCon' _) acc = acc
--   go bound (Fun' arg res) acc = go bound arg (go bound res acc)
--   go bound (ForAll' tvs ty) acc = go (tvs ++ bound) ty acc

-- tyVarBndrs :: Rho ann -> [TyVar ann]
-- -- Get all the binders used in ForAlls in the type, so that
-- -- when quantifying an outer for-all we can avoid these inner ones
-- tyVarBndrs ty = nub (bndrs ty)
--  where
--   bndrs (ForAll' tvs body) = tvs ++ bndrs body
--   bndrs (Fun' arg res) = bndrs arg ++ bndrs res
--   bndrs _ = []

-- tyVarName :: TyVar ann -> Text
-- tyVarName (BoundTv _ann n) = n
-- tyVarName (SkolemTv _ann n _) = n

-- ---------------------------------
-- --      Substitution

-- type Env ann = [(TyVar ann, Tau ann)]

-- substTy :: [TyVar (Maybe ann)] -> [Type (Maybe ann)] -> Type (Maybe ann) -> Type (Maybe ann)
-- -- Replace the specified quantified Type ann variables by
-- -- given meta Type ann variables
-- -- No worries about capture, because the two kinds of type
-- -- variable are distinct
-- substTy tvs tys ty = subst_ty (tvs `zip` tys) ty

-- subst_ty :: Env ann -> Type ann -> Type ann
-- subst_ty env (Fun ann arg res) = Fun ann (subst_ty env arg) (subst_ty env res)
-- subst_ty env (TyVar ann n) = fromMaybe (TyVar ann n) (lookup n env)
-- -- subst_ty _env (MetaTv ann tv) = MetaTv ann tv
-- subst_ty _env (TyCon ann tc) = TyCon ann tc
-- subst_ty env (ForAll ann ns rho) = ForAll ann ns (subst_ty env' rho)
--  where
--   env' = [(n, ty') | (n, ty') <- env, not (n `elem` ns)]

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

-- pprName :: Name (Maybe ann) -> Doc ann
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
