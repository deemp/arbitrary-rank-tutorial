module Language.Arralac.Zonker.Zn.Zonk where

import Control.Lens ((%~))
import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.IORef (readIORef)
import GHC.Stack (HasCallStack)
import Language.Arralac.Prelude.Debug (debug')
import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Prelude.Types
import Language.Arralac.Syntax.Local.Anno ()
import Language.Arralac.Syntax.Local.Extension.Tc
import Language.Arralac.Syntax.Local.Extension.Zn
import Language.Arralac.Syntax.Local.SynTerm.Tc ()
import Language.Arralac.Syntax.Local.SynTerm.Zn ()
import Language.Arralac.Syntax.Local.SynTermVar.Tc
import Language.Arralac.Syntax.Local.SynTermVar.Zn
import Language.Arralac.Syntax.Local.SynType.Tc ()
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Syntax.Local.TyVar.Zn
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Syntax.TTG.Type

-- Substitute metavariables with their Indirect types after typechecking
-- to produce a term without metavariables.
--
-- See Note [What is zonking?] in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Zonk/Type.hs#L106

type ZnM a = (HasCallStack, CtxDebug, CtxPrettyVerbosity) => IO a

class Zonk a where
  type ZonkTo a
  zonk :: a -> ZnM (ZonkTo a)

instance Zonk TcTyVar where
  type ZonkTo TcTyVar = Either ZnTyVar ZnType
  zonk var =
    case var.varDetails of
      MetaTv{metaTvRef} -> do
        debug'
          "zonk TcTyVar - MetaTv"
          [ ("var", pretty' var)
          ]
        metaDetails <- readIORef metaTvRef
        case metaDetails of
          Flexi -> do
            let name' = var.varName & #nameOcc . #occNameFS %~ \x -> x <> "_Unsolved"
            pure $ Left ZnTyVar{varName = name'}
          Indirect ty -> do
            ty' <- zonk ty
            pure $ Right ty'
      _ -> do
        debug'
          "zonk TcTyVar - not MetaTv"
          [ ("var", pretty' var)
          ]
        pure $ Left ZnTyVar{varName = var.varName}

instance Zonk (SynType CompTc) where
  type ZonkTo (SynType CompTc) = SynType CompZn
  zonk = \case
    -- TODO get type from tctyvar
    SynType'Var anno TcTyVar{varName} -> do
      pure $ SynType'Var anno ZnTyVar{varName}
    (SynType'ForAll srcLoc vars body) -> do
      (vars', _tys) <- partitionEithers <$> forM vars zonk
      -- TODO handle _tys non-empty
      body' <- zonk body
      pure $ SynType'ForAll srcLoc vars' body'
    SynType'Fun srcLoc arg res -> do
      arg' <- zonk arg
      res' <- zonk res
      pure $ SynType'Fun srcLoc arg' res'
    SynType'Concrete anno lit -> do
      pure $ SynType'Concrete anno lit

instance Zonk TcType where
  type ZonkTo TcType = ZnType
  zonk = \case
    -- TODO are zonks of different parts of a type independent?
    Type'Var var -> do
      var' <- zonk var
      pure $ either Type'Var id var'
    Type'ForAll vars body -> do
      (vars', _tys) <- partitionEithers <$> forM vars zonk
      -- TODO handle _tys non-empty
      body' <- zonk body
      pure $ Type'ForAll vars' body'
    Type'Fun arg res -> do
      arg' <- zonk arg
      res' <- zonk res
      pure $ Type'Fun arg' res'
    Type'Concrete ty -> do
      pure $ Type'Concrete ty

instance Zonk TcAnno where
  type ZonkTo TcAnno = ZnAnno
  zonk TcAnno{annoSrcLoc, annoType} = do
    annoType' <- zonk annoType
    pure ZnAnno{annoSrcLoc, annoType = annoType'}

instance Zonk TcTermVar where
  type ZonkTo TcTermVar = ZnTermVar
  zonk TcTermVar{varName, varType} = do
    varType' <- zonk varType
    pure ZnTermVar{varName, varType = varType'}

instance Zonk (Expected TcType) where
  type ZonkTo (Expected TcType) = ZnType
  zonk ty = do
    varType' <-
      case ty of
        Infer r -> readIORef r
        Check t -> pure t
    zonk varType'

instance Zonk (SynTerm CompTc) where
  type ZonkTo (SynTerm CompTc) = SynTerm CompZn
  zonk = \case
    SynTerm'Var _ var -> do
      var' <- zonk var
      pure $ SynTerm'Var () var'
    SynTerm'Lit anno lit -> do
      anno' <- zonk anno
      pure $ SynTerm'Lit anno' lit
    SynTerm'App anno fun arg -> do
      anno' <- zonk anno
      fun' <- zonk fun
      arg' <- zonk arg
      pure $ SynTerm'App anno' fun' arg'
    SynTerm'Lam anno var ty -> do
      anno' <- zonk anno
      var' <- zonk var
      ty' <- zonk ty
      pure $ SynTerm'Lam anno' var' ty'
    SynTerm'ALam anno var ty body -> do
      anno' <- zonk anno
      var' <- zonk var
      ty' <- zonk ty
      body' <- zonk body
      pure $ SynTerm'ALam anno' var' ty' body'
    SynTerm'Let anno var val term -> do
      anno' <- zonk anno
      var' <- zonk var
      val' <- zonk val
      term' <- zonk term
      pure $ SynTerm'Let anno' var' val' term'
    SynTerm'Ann anno term ty -> do
      anno' <- zonk anno
      term' <- zonk term
      ty' <- zonk ty
      pure $ SynTerm'Ann anno' term' ty'
