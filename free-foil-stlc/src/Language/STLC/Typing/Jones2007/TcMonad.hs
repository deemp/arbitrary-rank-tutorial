module Language.STLC.Typing.Jones2007.TcMonad (
  Tc, -- The monad type constructor
  runTc,
  ErrMsg,
  lift,
  check,
  -- Environment manipulation
  extendVarEnv,
  lookupVar,
  getEnvTypes,
  getFreeTyVars,
  getMetaTyVars,
  -- Types and unification
  newTyVarTy,
  instantiate,
  skolemise,
  zonkType,
  quantify,
  unify,
  unifyFun,
  -- Ref cells
  newTcRef,
  readTcRef,
  writeTcRef,
) where

import Data.IORef
import Data.List ((\\))
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Language.STLC.Typing.Jones2007.BasicTypes
import Prettyprinter

------------------------------------------
--      The monad itself                --
------------------------------------------

data TcEnv ann
  = TcEnv
  { uniqs :: IORef Uniq -- Unique supply
  , var_env :: Map.Map (Name (Maybe ann)) (Sigma (Maybe ann)) -- (Type ann) environment for term variables
  }

newtype Tc ann a = Tc (TcEnv ann -> IO (Either (ErrMsg) a))

unTc :: Tc ann a -> (TcEnv ann -> IO (Either (ErrMsg) a))
unTc (Tc a) = a

type ErrMsg = Doc Text

instance Functor (Tc ann) where
  fmap f x =
    Tc
      ( \env -> do
          x1 <- unTc x env
          case x1 of
            Left err -> return (Left err)
            Right x2 -> return (Right (f x2))
      )

instance Applicative (Tc ann) where
  pure x = Tc (\_env -> return (Right x))
  f <*> x =
    Tc
      ( \env -> do
          f1 <- unTc f env
          case f1 of
            Left err -> return (Left err)
            Right f2 -> do
              x1 <- unTc x env
              case x1 of
                Left err -> return (Left err)
                Right x2 -> return (Right (f2 x2))
      )

instance Monad (Tc ann) where
  return = pure
  m >>= k =
    Tc
      ( \env -> do
          r1 <- unTc m env
          case r1 of
            Left err -> return (Left err)
            Right v -> unTc (k v) env
      )

instance MonadFail (Tc ann) where
  fail err = Tc (\_env -> return (Left (pretty err)))

failTc :: Doc ann -> Tc ann a -- Fail unconditionally
failTc d = fail (docToString d)

check :: Bool -> Doc ann -> Tc ann ()
check True _ = return ()
check False d = failTc d

runTc :: [(Name (Maybe ann), Sigma (Maybe ann))] -> Tc ann a -> IO (Either (ErrMsg) a)
-- Run type-check, given an initial environment
runTc binds (Tc tc) =
  do
    ref <- newIORef 0
    let env =
          TcEnv
            { uniqs = ref
            , var_env = Map.fromList binds
            }
    tc env
 where

lift :: IO a -> Tc ann a
-- Lift a state transformer action into the typechecker monad
-- ignores the environment and always succeeds
lift st = Tc (\_env -> do r <- st; return (Right r))

newTcRef :: a -> Tc ann (IORef a)
newTcRef v = lift (newIORef v)

readTcRef :: IORef a -> Tc ann a
readTcRef r = lift (readIORef r)

writeTcRef :: IORef a -> a -> Tc ann ()
writeTcRef r v = lift (writeIORef r v)

--------------------------------------------------
--      Dealing with the type environment       --
------------------------------------------------- -

extendVarEnv :: (Name (Maybe ann)) -> (Sigma (Maybe ann)) -> Tc ann a -> Tc ann a
extendVarEnv var ty (Tc m) =
  Tc (\env -> m (extend env))
 where
  extend env = env{var_env = Map.insert var ty (var_env env)}

getEnv :: Tc ann (Map.Map (Name (Maybe ann)) (Sigma (Maybe ann)))
getEnv = Tc (\env -> return (Right (var_env env)))

lookupVar :: (Name (Maybe ann)) -> Tc ann (Sigma (Maybe ann)) -- May fail
lookupVar n = do
  env <- getEnv
  case Map.lookup n env of
    Just ty -> return ty
    Nothing -> failTc (pretty "Not in scope:" <+> dquotes (pprName n))

--------------------------------------------------
--      Creating, reading, writing MetaTvs        --
--------------------------------------------------

newTyVarTy :: Tc ann (Tau (Maybe ann))
newTyVarTy = do
  tv <- newMetaTyVar
  return (MetaTv Nothing tv)

newMetaTyVar :: Tc ann (MetaTv (Maybe ann))
newMetaTyVar = do
  uniq <- newUnique
  tref <- newTcRef Nothing
  return (Meta Nothing uniq tref)

newSkolemTyVar :: (TyVar (Maybe ann)) -> Tc ann (TyVar (Maybe ann))
newSkolemTyVar tv = do
  uniq <- newUnique
  return (SkolemTv Nothing (tyVarName tv) uniq)

readTv :: (MetaTv (Maybe ann)) -> Tc ann (Maybe (Tau (Maybe ann)))
readTv (Meta _ _ ref) = readTcRef ref

writeTv :: (MetaTv (Maybe ann)) -> (Tau (Maybe ann)) -> Tc ann ()
writeTv (Meta _ _ ref) ty = writeTcRef ref (Just ty)

newUnique :: Tc ann Uniq
newUnique =
  Tc
    ( \(TcEnv{uniqs = ref}) ->
        do
          uniq <- readIORef ref
          writeIORef ref (uniq + 1)
          return (Right uniq)
    )

------------------------------------------
--      Instantiation                   --
------------------------------------------

instantiate :: (Sigma (Maybe ann)) -> Tc ann (Rho (Maybe ann))
-- Instantiate the topmost for-alls of the argument type
-- with flexible type variables
instantiate (ForAll' tvs ty) =
  do
    tvs' <- mapM (\_ -> newMetaTyVar) tvs
    -- TODO is it ok to use `pure` here?
    return (substTy tvs (map (MetaTv Nothing) (tvs')) (ty))
instantiate ty =
  return (ty)

skolemise :: (Sigma (Maybe ann)) -> Tc ann ([TyVar (Maybe ann)], Rho (Maybe ann))
-- Performs deep skolemisation, retuning the
-- skolem constants and the skolemised type
skolemise (ForAll' tvs ty) -- Rule PRPOLY
  =
  do
    sks1 <- mapM newSkolemTyVar tvs
    (sks2, ty') <- skolemise (substTy tvs (map (TyVar Nothing) sks1) ty)
    return (sks1 ++ sks2, ty')
skolemise (Fun ann arg_ty res_ty) -- Rule PRFUN
  =
  do
    (sks, res_ty') <- skolemise res_ty
    return (sks, Fun ann arg_ty res_ty')
skolemise ty -- Rule PRMONO
  =
  return ([], ty)

------------------------------------------
--      Quantification                  --
------------------------------------------

quantify :: [(MetaTv (Maybe ann))] -> (Rho (Maybe ann)) -> Tc ann (Sigma (Maybe ann))
-- Quantify over the specified type variables (all flexible)
quantify tvs ty =
  do
    mapM_ bind (tvs `zip` new_bndrs) -- 'bind' is just a cunning way
    ty' <- zonkType ty -- of doing the substitution
    return (ForAll Nothing new_bndrs ty')
 where
  used_bndrs = tyVarBndrs ty -- Avoid quantified type variables in use
  new_bndrs = take (length tvs) (allBinders \\ used_bndrs)
  bind (tv, name) = writeTv tv ((TyVar Nothing) name)

allBinders :: [(TyVar (Maybe ann))] -- a,b,..z, a1, b1,... z1, a2, b2,...
allBinders =
  [BoundTv Nothing (pack [x]) | x <- ['a' .. 'z']]
    ++ [BoundTv Nothing (pack (x : show i)) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]

------------------------------------------
--      Getting the free tyvars         --
------------------------------------------

getEnvTypes :: Tc ann [(Type (Maybe ann))]
-- Get the types mentioned in the environment
getEnvTypes = do
  env <- getEnv
  return (Map.elems env)

getMetaTyVars :: [(Type (Maybe ann))] -> Tc ann [(MetaTv (Maybe ann))]
-- This function takes account of zonking, and returns a set
-- (no duplicates) of unbound meta-type variables
getMetaTyVars tys = do
  tys' <- mapM zonkType tys
  return (metaTvs tys')

getFreeTyVars :: [(Type (Maybe ann))] -> Tc ann [(TyVar (Maybe ann))]
-- This function takes account of zonking, and returns a set
-- (no duplicates) of free type variables
getFreeTyVars tys = do
  tys' <- mapM zonkType tys
  return (freeTyVars tys')

------------------------------------------
--      Zonking                         --
-- Eliminate any substitutions in the type
------------------------------------------

zonkType :: (Type (Maybe ann)) -> Tc ann (Type (Maybe ann))
zonkType (ForAll ann ns ty) = do
  ty' <- zonkType ty
  return (ForAll ann ns ty')
zonkType (Fun ann arg res) = do
  arg' <- zonkType arg
  res' <- zonkType res
  return (Fun ann arg' res')
zonkType (TyCon ann tc) = return (TyCon ann tc)
zonkType (TyVar ann n) = return (TyVar ann n)
zonkType (MetaTv ann tv) -- A mutable type variable
  =
  do
    mb_ty <- readTv tv
    case mb_ty of
      Nothing -> return ((MetaTv ann) tv)
      Just ty -> do
        ty' <- zonkType ty
        writeTv tv ty' -- "Short out" multiple hops
        return ty'

------------------------------------------
--      Unification                     --
------------------------------------------

unify :: (Tau (Maybe ann)) -> (Tau (Maybe ann)) -> Tc ann ()
unify ty1 ty2
  | badType ty1 || badType ty2 -- Compiler error
    =
      failTc
        ( pretty "Panic! Unexpected types in unification:"
            <+> vcat [ppr ty1, ppr ty2]
        )
unify (TyVar' tv1) (TyVar' tv2) | tv1 == tv2 = return ()
unify (MetaTv' tv1) (MetaTv' tv2) | tv1 == tv2 = return ()
unify (MetaTv' tv) ty = unifyVar tv ty
unify ty (MetaTv' tv) = unifyVar tv ty
unify
  (Fun' arg1 res1)
  (Fun' arg2 res2) =
    do unify arg1 arg2; unify res1 res2
unify (TyCon' tc1) (TyCon' tc2)
  | tc1 == tc2 =
      return ()
unify ty1 ty2 = failTc (pretty "Cannot unify types:" <+> vcat [ppr ty1, ppr ty2])

-----------------------------------------
unifyVar :: (MetaTv (Maybe ann)) -> (Tau (Maybe ann)) -> Tc ann ()
-- Invariant: tv1 is a flexible type variable
unifyVar tv1 ty2 -- Check whether tv1 is bound
  =
  do
    mb_ty1 <- readTv tv1
    case mb_ty1 of
      Just ty1 -> unify ty1 ty2
      Nothing -> unifyUnboundVar tv1 ty2

unifyUnboundVar :: (MetaTv (Maybe ann)) -> (Tau (Maybe ann)) -> Tc ann ()
-- Invariant: the flexible type variable tv1 is not bound
unifyUnboundVar tv1 ty2@(MetaTv' tv2) =
  do
    -- We know that tv1 /= tv2 (else the
    -- top case in unify would catch it)
    mb_ty2 <- readTv tv2
    case mb_ty2 of
      -- TODO which ann to use?
      Just ty2' -> unify (MetaTv Nothing tv1) ty2'
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
unifyFun :: Rho (Maybe ann) -> Tc ann (Sigma (Maybe ann), Rho (Maybe ann))
--      (arg,res) <- unifyFunTy fun
-- unifies 'fun' with '(arg -> res)'
unifyFun (Fun' arg res) = return (arg, res)
unifyFun tau = do
  arg_ty <- newTyVarTy
  res_ty <- newTyVarTy
  unify tau (Fun Nothing arg_ty res_ty)
  return (arg_ty, res_ty)

-----------------------------------------
occursCheckErr :: (MetaTv (Maybe ann)) -> (Tau (Maybe ann)) -> Tc ann ()
-- Raise an occurs-check error
occursCheckErr tv ty =
  failTc
    ( pretty "Occurs check for"
        <+> dquotes (ppr tv)
        <+> pretty "in:"
        <+> ppr ty
    )

badType :: (Tau ann) -> Bool
-- Tells which types should never be encountered during unification
badType (TyVar' (BoundTv' _)) = True
badType _ = False
