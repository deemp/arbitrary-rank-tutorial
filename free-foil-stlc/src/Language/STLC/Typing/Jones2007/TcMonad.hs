{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordPuns #-}
{-# LANGUAGE TupleSections #-}

module Language.STLC.Typing.Jones2007.TcMonad 
-- (
--   TcM, -- The monad type constructor
--   -- runTc,
--   -- ErrMsg,
--   lift,
--   check,
--   -- Environment manipulation
--   extendVarEnv,
--   lookupVar,
--   getEnvTypes,
--   getFreeTyVars,
--   getMetaTyVars,
--   -- Types and unification
--   -- newTyVarTy,
--   instantiate,
--   skolemise,
--   zonkType,
--   quantify,
--   unify,
--   unifyFun,
--   -- Ref cells
--   newTcRef,
--   readTcRef,
--   writeTcRef,
-- ) 
where

import Data.IORef
import Data.List (partition, (\\))
import Data.Map qualified as Map
import Data.Text (pack)
import Data.Traversable (forM)
import Language.STLC.Typing.Jones2007.BasicTypes
import Prettyprinter
import Prettyprinter.Render.Text

------------------------------------------
--      The monad itself                --
------------------------------------------

-- data TcEnv
--   = TcEnv
--   { uniqs :: IORef Unique -- Unique supply
--   , var_env :: Map.Map Name Sigma -- (Type) environment for term variables
--   }

type IVarEnv = (?varEnv :: Map.Map Name Sigma)
type ITcLevel = (?tcLevel :: TcLevel)

type ITcEnv = (IUniqueSupply, IVarEnv, ITcLevel)

-- TcM in GHC is a ReaderT (Env a) IO b.
-- It can be replaced with ImplicitParams
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types.hs#L271
type TcM a = (ITcEnv) => IO a

-- TODO fail with a different message?
failTc :: Doc ann -> TcM a -- Fail unconditionally
failTc d = do
  putDoc d
  fail "Error!"

-- TODO use somewhere
check :: Bool -> Doc ann -> TcM ()
check True _ = pure ()
check False d = failTc d

-- TODO explain optimizations

-- TODO adapt?
lift :: IO a -> TcM a
-- Lift a state transformer action into the typechecker monad
-- ignores the environment and always succeeds
lift st = st

-- TcM (\_env -> do r <- st; pure (Right r))

newTcRef :: a -> TcM (IORef a)
newTcRef v = lift (newIORef v)

readTcRef :: IORef a -> TcM a
readTcRef r = lift (readIORef r)

writeTcRef :: IORef a -> a -> TcM ()
writeTcRef r v = lift (writeIORef r v)

--------------------------------------------------
--      Dealing with the type environment       --
------------------------------------------------- -

extendVarEnv :: Name -> Sigma -> TcM a -> TcM a
extendVarEnv var ty m =
  let ?varEnv = Map.insert var ty ?varEnv in m

getEnv :: TcM (Map.Map Name Sigma)
getEnv = pure ?varEnv

-- TcM (\env -> pure (Right (var_env env)))

lookupVar :: Name -> TcM Sigma -- May fail
lookupVar n = do
  env <- getEnv
  case Map.lookup n env of
    Just ty -> pure ty
    Nothing -> failTc ("Not in scope:" <+> dquotes (pprName n))

--------------------------------------------------
--      Creating, reading, writing MetaTvs        --
--------------------------------------------------

-- TODO pattern synonym for MetaTv

-- TODO rename to use Meta
-- newTyVarTy :: TcM Tau
-- newTyVarTy = do
--   -- TODO use a supplied name
--   tv <- newMetaTyVar "a"
--   pure (Type'Var tv)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L552
mkSystemNameAt :: Unique -> OccName -> SrcSpan -> Name
mkSystemNameAt uniq occ loc =
  Name
    { nameUnique = uniq
    , nameOcc = occ
    , nameLoc = loc
    }

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/SrcLoc.hs#L465
noSrcSpan :: SrcSpan
noSrcSpan = UnhelpfulSpan UnhelpfulNoLocationInfo

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name.hs#L548
mkSystemName :: Unique -> OccName -> Name
mkSystemName uniq occ = mkSystemNameAt uniq occ noSrcSpan

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L261
tvName :: NameSpace
tvName = NameSpace'Type'Var

mkOccNameFS :: NameSpace -> FastString -> OccName
mkOccNameFS occ_sp fs = OccName occ_sp fs

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Name/Occurrence.hs#L488
mkTyVarOccFS :: FastString -> OccName
mkTyVarOccFS fs = mkOccNameFS tvName fs

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Monad.hs#L735
newSysName :: OccName -> TcM Name
newSysName occ =
  do
    uniq <- newUnique
    pure (mkSystemName uniq occ)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L728
newMetaTyVarName :: FastString -> TcM Name
-- Makes a /System/ Name, which is eagerly eliminated by
-- the unifier; see GHC.Tc.Utils.Unify.nicer_to_update_tv1, and
-- GHC.Tc.Solver.Equality.canEqTyVarTyVar (nicer_to_update_tv2)
newMetaTyVarName str =
  newSysName (mkTyVarOccFS str)

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Data/IOEnv.hs#L218
newMutVar :: a -> TcM (IORef a)
newMutVar val = newIORef val

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L844
newMetaDetails :: MetaInfo -> TcM TcTyVarDetails
newMetaDetails info =
  do
    ref <- newMutVar Flexi
    pure
      MetaTv
        { metaTvInfo = info
        , metaTvRef = ref
        , metaTvTcLevel = ?tcLevel
        }

-- Similar to `newMetaTyVarTyAtLevel`
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1069
newMetaTyVar' :: FastString -> TcM TcTyVarMeta
newMetaTyVar' str = do
  name <- newMetaTyVarName str
  details <- newMetaDetails TyVarTv
  pure $ mkTcTyVar name details

-- TODO how to remember the origin of a metavariable?

-- It's usually `newMetaTyVarName` + `mkTcTyVar`
tyVarToMetaTyVar :: TcTyVar -> TcM TcTyVar
tyVarToMetaTyVar TyVar{varName} = newMetaTyVar' varName.nameOcc.occNameFS
tyVarToMetaTyVar tv = failTc $ "Expected a TyVar, but got: " <> pretty tv

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L1046
mkTcTyVar :: Name -> TcTyVarDetails -> TyVar
mkTcTyVar name details =
  -- NB: 'kind' may be a coercion kind; cf, 'GHC.Tc.Utils.TcMType.newMetaCoVar'
  TcTyVar
    { varName = name
    , varDetails = details
    }

-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L765
newSkolemTyVar' :: (ITcLevel) => SkolemInfo -> Name -> TcTyVar
newSkolemTyVar' info name = mkTcTyVar name (SkolemTv info ?tcLevel)

-- TODO use `uniqFromTag`
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique/Supply.hs#L282

-- Similar to `cloneTyVarTyVar`
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L779
newSkolemTyVar :: SkolemInfoAnon -> TyVar -> TcM TyVar
newSkolemTyVar infoAnon tv@TyVar{} = do
  uniq <- newUnique
  let name = tv.varName{nameUnique = uniq}
      -- TODO should this skolemInfo contain the uniq of the original tyvar?
      -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L343
      skolemInfo = SkolemInfo tv.varName.nameUnique infoAnon
  pure $ newSkolemTyVar' skolemInfo name
-- TODO improve message
newSkolemTyVar infoAnon _tv =
  failTc ("Expected a TyVar, but got: " <> pretty _tv <> " in " <> pretty infoAnon)

-- `metaTyVarRef` panics if a tv doesn't have a ref
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L1317

-- TODO not maybe, but Flexi and Indirect
readTv :: TcTyVarMeta -> TcM (Maybe Tau)
readTv (TcTyVar{varDetails = MetaTv{metaTvRef = ref}}) = do
  details <- readTcRef ref
  pure $ case details of
    Flexi -> Nothing
    Indirect ty -> Just ty
readTv _ = pure Nothing

writeTv :: TcTyVarMeta -> Tau -> TcM ()
writeTv m@(TcTyVar{varDetails = MetaTv{metaTvRef = ref}}) ty =
  readTcRef ref >>= \case
    Flexi -> writeTcRef ref (Indirect ty)
    -- TODO improve message
    _ -> failTc ("Expected unfilled variable, but got: " <> pretty m)
writeTv _ _ = pure ()

------------------------------------------
--      Instantiation                   --
------------------------------------------

instantiate :: Sigma -> TcM Rho
-- Instantiate the topmost for-alls of the argument type
-- with flexible type variables
instantiate (Type'ForAll tvs ty) = do
  tvs' <- forM tvs tyVarToMetaTyVar
  pure (substTy tvs (Type'Var <$> tvs') ty)
instantiate ty = pure ty

skolemise :: Sigma -> TcM ([TyVar], Rho)
-- Performs deep skolemisation, retuning the
-- skolem constants and the skolemised type
-- Rule PRPOLY
skolemise (Type'ForAll tvs ty) = do
  let info = ForAllSkol tvs
  sks1 <- mapM (newSkolemTyVar info) tvs
  (sks2, ty') <- skolemise (substTy tvs (Type'Var <$> sks1) ty)
  pure (sks1 <> sks2, ty')
-- Rule PRFUN
skolemise (Type'Fun arg_ty res_ty) = do
  (sks, res_ty') <- skolemise res_ty
  pure (sks, Type'Fun arg_ty res_ty')
-- Rule PRMONO
skolemise ty = pure ([], ty)

------------------------------------------
--      Quantification                  --
------------------------------------------

isMetaTv :: Var -> Bool
isMetaTv TcTyVar{varDetails = MetaTv{}} = True
isMetaTv _ = False

isFlexiTv :: Var -> TcM Bool
isFlexiTv TcTyVar{varDetails = MetaTv{metaTvRef}} =
  readTcRef metaTvRef >>= \case
    Flexi -> pure True
    _ -> pure False
isFlexiTv _ = pure False

-- TODO should `quantify` use `SkolemTv` or `TyVar` in forall?
-- TyVarDetails, MetaDetails, MetaInfo
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L549

-- As written in Sec. 5.5
-- > Furthermore, quantify guarantees to return a type that is fully substituted;
-- this makes it easier to instantiate later, because the proper type variables
-- can all be found without involving the substitution.
--
-- Hence, `quantify` should probably use `Id` in forall

-- TODO account for levels
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1677
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1750
quantify :: [TcTyVar] -> Rho -> TcM Sigma
-- Quantify over the specified type variables (all flexible)
quantify tvs ty | all isMetaTv tvs = do
  -- TODO remove this check?
  tvs' <- forM tvs (\x -> (x,) <$> isFlexiTv x) >>= pure . partition snd
  case tvs' of
    (_, x : xs) -> failTc ("Expected all variables to be Flexi, but these are not: " <> pretty (x : xs))
    _ -> pure ()
  -- 'bind' is just a cunning way of doing the substitution
  -- TODO is it correct to use these vars in ForAll?
  vars <- mapM bind (tvs `zip` newBinders)
  ty' <- zonkType ty
  pure (Type'ForAll vars ty')
 where
  -- TODO optimize
  usedBinders = tyVarBndrs ty
  -- -- Avoid quantified type variables in use
  newBinders :: [FastString]
  newBinders = take (length tvs) (allBinders \\ ((.varName.nameOcc.occNameFS) <$> usedBinders))
  bind :: (TcTyVarMeta, FastString) -> TcM Var
  bind (tv, name) = do
    -- TODO use info from tv?
    name' <- newSysName (mkTyVarOccFS name)
    let var = Id{varName = name'}
    writeTv tv (Type'Var var)
    pure var
quantify tvs _ = failTc $ "Expected all type variables to not be metavariables, but got: " <> pretty tvs

-- GHC uses tags, IIUC
-- TODO link to explanation
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Core/Opt/Monad.hs#L125
allBinders :: [FastString] -- a,b,..z, a1, b1,... z1, a2, b2,...
allBinders =
  [(pack [x]) | x <- ['a' .. 'z']]
    <> [ (pack (x : show i))
       | i <- [1 :: Integer ..]
       , x <- ['a' .. 'z']
       ]

------------------------------------------
--      Getting the free tyvars         --
------------------------------------------

getEnvTypes :: TcM [Type]
-- Get the types mentioned in the environment
getEnvTypes = do
  env <- getEnv
  pure (Map.elems env)

getMetaTyVars :: [Type] -> TcM [TcTyVar]
-- This function takes account of zonking, and returns a set
-- (no duplicates) of unbound meta-type variables
getMetaTyVars tys = do
  tys' <- mapM zonkType tys
  pure (metaTvs tys')

getFreeTyVars :: [Type] -> TcM [TyVar]
-- This function takes account of zonking, and returns a set
-- (no duplicates) of free type variables
getFreeTyVars tys = do
  tys' <- mapM zonkType tys
  pure (freeTyVars tys')

------------------------------------------
--      Zonking                         --
-- Eliminate any substitutions in the type
------------------------------------------

-- TODO backsubstitution
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Zonk/Type.hs#L925


zonkType :: Type -> TcM Type
zonkType (Type'ForAll ns ty) = do
  ty' <- zonkType ty
  pure (Type'ForAll ns ty')
zonkType (Type'Fun arg res) = do
  arg' <- zonkType arg
  res' <- zonkType res
  pure (Type'Fun arg' res')
zonkType (Type'Concrete tc) = pure (Type'Concrete tc)
zonkType t@(Type'Var var) =
  case var of
    TcTyVar{varDetails = MetaTv{}} ->
      do
        mb_ty <- readTv var
        case mb_ty of
          -- TODO why does zonking keep metavariables with flexi types?
          Nothing -> pure t
          Just ty -> do
            ty' <- zonkType ty
            -- "Short out" multiple hops
            writeTv var ty'
            pure ty'
    _ -> pure (Type'Var var)

------------------------------------------
--      Unification                     --
------------------------------------------

unify :: Tau -> Tau -> TcM ()
unify ty1 ty2
  | badType ty1 || badType ty2 -- Compiler error
    =
      failTc
        ( "Panic! Unexpected types in unification:"
            <+> vcat [pretty ty1, pretty ty2]
        )
unify (Type'Var TcTyVar{varDetails = MetaTv{metaTvRef = tv1}}) (Type'Var TcTyVar{varDetails = MetaTv{metaTvRef = tv2}}) | tv1 == tv2 = pure ()
unify (Type'Var tv@TcTyVar{varDetails = MetaTv{}}) ty = unifyVar tv ty
unify ty (Type'Var tv@TcTyVar{varDetails = MetaTv{}}) = unifyVar tv ty
-- TODO is equality defined correctly?
unify (Type'Var tv1@TcTyVar{}) (Type'Var tv2@TcTyVar{}) | tv1 == tv2 = pure ()
unify (Type'Fun arg1 res1) (Type'Fun arg2 res2) = do
  unify arg1 arg2
  unify res1 res2
unify (Type'Concrete tc1) (Type'Concrete tc2) | tc1 == tc2 = pure ()
unify ty1 ty2 = failTc ("Cannot unify types:" <+> vcat [pretty ty1, pretty ty2])

-----------------------------------------
unifyVar :: TcTyVar -> Tau -> TcM ()
-- Invariant: tv1 is a flexible type variable
-- Check whether tv1 is bound
unifyVar tv1 ty2 | isMetaTv tv1 = do
  mb_ty1 <- readTv tv1
  case mb_ty1 of
    Just ty1 -> unify ty1 ty2
    Nothing -> unifyUnboundVar tv1 ty2
unifyVar _ _ = pure ()

unifyUnboundVar :: TcTyVar -> Tau -> TcM ()
-- Invariant: the flexible type variable tv1 is not bound
unifyUnboundVar tv1 ty2@((Type'Var tv2@TcTyVar{varDetails = MetaTv{}})) =
  do
    -- We know that tv1 /= tv2 (else the
    -- top case in unify would catch it)
    mb_ty2 <- readTv tv2
    case mb_ty2 of
      Just ty2' -> unify (Type'Var tv1) ty2'
      Nothing -> writeTv tv1 ty2
unifyUnboundVar tv1 ty2 =
  do
    tvs2 <- getMetaTyVars [ty2]
    if tv1 `elem` tvs2
      then
        occursCheckErr tv1 ty2
      else
        writeTv tv1 ty2

-----------------------------------------
unifyFun :: Rho -> TcM (Sigma, Rho)
--      (arg,res) <- unifyFunTy fun
-- unifies 'fun' with '(arg -> res)'
unifyFun (Type'Fun arg res) = pure (arg, res)
unifyFun tau = do
  -- TODO use better names?
  arg_ty <- Type'Var <$> newMetaTyVar' "a"
  res_ty <- Type'Var <$> newMetaTyVar' "b"
  unify tau (Type'Fun arg_ty res_ty)
  pure (arg_ty, res_ty)

-----------------------------------------
occursCheckErr :: TcTyVar -> Tau -> TcM ()
-- Raise an occurs-check error
occursCheckErr tv ty =
  failTc
    ( "Occurs check for"
        <+> dquotes (pretty tv)
        <+> "in:"
        <+> pretty ty
    )

badType :: Tau -> Bool
-- Tells which types should never be encountered during unification
badType (Type'Var (TyVar _)) = True
badType (Type'Var (Id _ _)) = True
badType _ = False
