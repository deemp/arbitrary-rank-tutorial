module Language.Arralac.Typechecker.TcTyVar where

import Data.IORef (IORef)
import GHC.IORef (newIORef)
import GHC.Stack (HasCallStack)
import Language.Arralac.Prelude.Debug
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Prelude.Unique
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.Type
import Language.Arralac.Typechecker.Error
import Language.Arralac.Typechecker.Types

type CtxTcTyVar =
  ( HasCallStack
  , CtxDebug
  , CtxTcLevel
  , CtxUniqueSupply
  , CtxPrettyVerbosity
  , CtxTcErrorPropagated
  )

-- ================
-- [Type variables]
-- ================

-- | Similar to @mkTcTyVar@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L1046
mkTcTyVar :: Name -> TcTyVarDetails -> TcTyVar
mkTcTyVar name details =
  TcTyVar
    { varName = name
    , varDetails = details
    }

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L261
tvName :: NameSpace
tvName = NameSpace'TyVar

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L488
mkTyVarOccFS :: FastString -> OccName
mkTyVarOccFS fs = mkOccNameFS tvName fs

mkOccNameFS :: NameSpace -> FastString -> OccName
mkOccNameFS occ_sp fs = OccName occ_sp fs

-- ======================
-- [Bound type variables]
-- ======================

isBoundTvTypeVar :: Tau -> Bool
-- Tells which types should never be encountered during unification
isBoundTvTypeVar (Type'Var TcTyVar{varDetails = BoundTv{}}) = True
isBoundTvTypeVar _ = False

-- ===============
-- [Metavariables]
-- ===============

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L728
newMetaTyVarName :: (CtxTcTyVar) => FastString -> IO Name
newMetaTyVarName str = newSysName (mkTyVarOccFS str)

-- | Similar to @newMutVar@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/IOEnv.hs#L218
newMutVar :: a -> IO (IORef a)
newMutVar val = newIORef val

-- | Similar to @newTauTvDetailsAtLevel@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L844
newMetaDetails :: (CtxTcTyVar) => MetaInfo -> IO TcTyVarDetails
newMetaDetails info =
  do
    ref <- newMutVar Flexi
    pure
      MetaTv
        { metaTvInfo = info
        , metaTvRef = ref
        , tcLevel = ?tcLevel
        }

-- | Similar to @newMetaTyVarTyAtLevel@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1069
newMetaTyVar' :: (CtxTcTyVar) => FastString -> IO TcTyVarMeta
newMetaTyVar' str = do
  name <- newMetaTyVarName str
  details <- newMetaDetails TyVarTv
  pure $ mkTcTyVar name details

-- TODO how to remember the origin of a metavariable?

tyVarToMetaTyVar :: (CtxTcTyVar) => TcTyVar -> IO TcTyVar
tyVarToMetaTyVar x = do
  x' <- newMetaTyVar' x.varName.nameOcc.occNameFS
  debug'
    "tyVarToMetaTyVar"
    [ ("x", pretty' x)
    , ("x'", pretty' x')
    ]
  pure x'

-- =========
-- [Skolems]
-- =========

-- | Makes a new skolem type variable.
--
-- Similar to @newSkolemTyVar@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L765
newSkolemTyVar' :: (CtxTcTyVar) => SkolemInfo -> Name -> TcTyVar
newSkolemTyVar' info name = mkTcTyVar name (SkolemTv info ?tcLevel)

-- | Converts a 'BoundTv' to a 'SkolemTv'.
--
-- Similar to @cloneTyVarTyVar@ in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L779
newSkolemTyVar :: (CtxTcTyVar) => SkolemInfoAnon -> TcBoundVar -> IO TcTyVar
newSkolemTyVar infoAnon tv@TcTyVar{varDetails = BoundTv{}} = do
  uniq <- newUnique
  let name = tv.varName{nameUnique = uniq}
      -- TODO should this skolemInfo contain the uniq of the original tyvar?
      -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L343
      skolemInfo = SkolemInfo tv.varName.nameUnique infoAnon
  pure $ newSkolemTyVar' skolemInfo name
newSkolemTyVar _ x =
  dieTc (TcError'UnboundVariable x)
