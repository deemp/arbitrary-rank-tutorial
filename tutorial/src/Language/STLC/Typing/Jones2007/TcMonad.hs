module Language.STLC.Typing.Jones2007.TcMonad where
  

import Control.Exception (Exception, throw)
import Control.Monad (when)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub, partition, (\\))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Traversable (forM)
import GHC.Records (HasField)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Language.STLC.Syntax.Abs qualified as Abs
import Language.STLC.Typing.Jones2007.Bag (Bag (..))
import Language.STLC.Typing.Jones2007.BasicTypes
import Language.STLC.Typing.Renamer
import Prettyprinter
import Prettyprinter.Util (putDocW)

-- ==============================================
-- The Type Checking Monad
-- ==============================================

type IVarEnv = (?varEnv :: Map.Map Name Sigma)
type ITcLevel = (?tcLevel :: TcLevel)
type IDebug = (?debug :: Bool)
type IConstraints = (?constraints :: IORef WantedConstraints)
type ITcErrorPropagated = (?tcErrorPropagated :: IORef (Maybe TcError))

type ITcEnv = (IUniqueSupply, IVarEnv, ITcLevel, IDebug, IConstraints, ITcErrorPropagated)

-- TcM in GHC is a ReaderT (Env a) IO b.
-- It can be replaced with ImplicitParams
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types.hs#L271

-- ==============================================
-- The Type Checking Errors
-- ==============================================

data TcError
  = TcError'UndefinedVariable {varName :: Name}
  | TcError'UnboundVariable {var :: TcTyVar}
  | TcError'UnexpectedType {expected :: TcType, actual :: TcType, thing :: Maybe TypedThing}
  | -- TODO specify which type(s) are bound
  | TcError'ExpectedFlexiVariables {tvs :: [TcTyVar]}
  | TcError'UnknownConcreteType {name :: Name}
  | TcError'ExpectedAllMetavariables {tvs :: [TcTyVar]}
  | TcError'CannotUnify {ty1 :: TcType, ty2 :: TcType, thing :: Maybe TypedThing}

-- Capture current callstack in GADT
-- https://maksbotan.github.io/posts/2021-01-20-callstacks.html
data TcErrorWithCallStack where
  TcErrorWithCallStack :: (HasCallStack) => TcError -> TcErrorWithCallStack

getThingStart :: TypedThing -> SrcSpan
getThingStart (HsExprRnThing thing) =
  case thing of
    SynTerm'Var _ var -> var.nameLoc
    SynTerm'Lit anno _ -> anno
    SynTerm'App anno _ _ -> anno
    SynTerm'Lam anno _ _ -> anno
    SynTerm'ALam anno _ _ _ -> anno
    SynTerm'Let anno _ _ _ -> anno
    SynTerm'Ann anno _ _ -> anno

prettyDefinedAt :: (IPrettyVerbosity) => TypedThing -> Doc ann
prettyDefinedAt thing =
  vsep'
    [ "defined at:"
    , prettyIndent (getThingStart thing)
    ]

instance Pretty' TcError where
  pretty' = \case
    TcError'UndefinedVariable{varName} ->
      vsep'
        [ "Not in scope:"
        , prettyIndent varName
        ]
    TcError'UnboundVariable{var} ->
      vsep'
        [ "Expected a bound variable, but got:"
        , prettyIndent var
        ]
    TcError'UnexpectedType{expected, actual, thing} ->
      vsep'
        [ "Expected the type:"
        , prettyIndent expected
        , "but got the type:"
        , prettyIndent actual
        , prettyThingLocation thing
        ]
    TcError'OccursCheck{ct} ->
      vsep'
        [ "Occurs check failed!"
        , "Type variable:"
        , prettyIndent ct.ct_eq_can.eq_lhs
        , "occurs in the type:"
        , prettyIndent ct.ct_eq_can.eq_rhs
        ]
    TcError'UnifyingBoundTypes{ty1, ty2, thing} ->
      vsep'
        [ "Trying to unify type:"
        , prettyIndent ty1
        , "with type:"
        , prettyIndent ty2
        , prettyThingLocation thing
        ]
    TcError'ExpectedFlexiVariables{tvs} ->
      vsep'
        [ "Expected all variables to be Flexi, but these are not:"
        , prettyIndent tvs
        ]
    TcError'UnknownConcreteType{name} ->
      vsep'
        [ "Unknown concrete type:"
        , prettyIndent name
        ]
    TcError'ExpectedAllMetavariables{tvs} ->
      vsep'
        [ "Expected all variables to be metavariables, but got:"
        , prettyIndent tvs
        ]
    TcError'CannotUnify{ty1, ty2, thing} ->
      vsep'
        [ "Cannot unify type:"
        , prettyIndent ty1
        , "with type:"
        , prettyIndent ty2
        , prettyThingLocation thing
        ]
   where
    prettyThingLocation :: Maybe TypedThing -> Doc ann
    prettyThingLocation = \case
      Nothing -> mempty
      Just thing ->
        vsep'
          [ "in the expression:"
          , prettyIndent thing
          , prettyDefinedAt thing
          ]

instance Pretty' TcErrorWithCallStack where
  pretty' (TcErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]

instance Exception TcError

instance Exception TcErrorWithCallStack

withTcError :: TcError -> TcM a -> TcM a
withTcError err tcAction = do
  tcErrorCurrent <- readIORef ?tcErrorPropagated
  case tcErrorCurrent of
    Nothing -> do
      writeIORef ?tcErrorPropagated (Just err)
      res <- tcAction
      writeIORef ?tcErrorPropagated Nothing
      pure res
    Just _ -> do
      tcAction

die :: (ITcErrorPropagated, HasCallStack) => TcError -> IO a -- Fail unconditionally
die tcErrorCurrent = do
  tcErrorPropagated <- readIORef ?tcErrorPropagated
  -- TODO Currently, a newer error can not overwrite the propagated error.
  -- We need this behavior to propagate actual vs expected type errors.
  -- If there's a mismatch during unification, we can immediately report.
  -- Should we choose the error for each combination of types of errors (new, propagated)?
  let error' =
        case tcErrorPropagated of
          Nothing -> tcErrorCurrent
          Just err -> err
  throw (TcErrorWithCallStack error')

-- ==============================================
-- Debugging Utility
-- ==============================================

debug :: (IDebug, IPrettyVerbosity) => Doc a -> [Doc a] -> IO ()
debug label xs = when ?debug do
  putDocW
    1000
    ( vsep
        [ "[" <> label <> "]"
        , foldMap (\x -> "$ " <> x <> line) xs
        , pretty' (prettyCallStack callStack)
        , line
        ]
    )

debug' :: (IDebug, IPrettyVerbosity) => Doc ann -> [(Doc ann, Doc ann)] -> IO ()
debug' label xs = debug label (prettyVarVal <$> xs)
 where
  prettyVarVal :: (Doc ann, Doc ann) -> Doc ann
  prettyVarVal (var, val) = var <> ":" <> line <> indent 4 val

-- ==============================================
-- Dealing with the type environment
-- ==============================================

extendVarEnv :: Name -> Sigma -> TcM a -> TcM a
extendVarEnv var ty tcAction =
  let ?varEnv = Map.insert var ty ?varEnv in tcAction

getEnv :: TcM (Map.Map Name Sigma)
getEnv = pure ?varEnv

lookupVar :: Name -> TcM Sigma -- May fail
lookupVar n =
  case Map.lookup n ?varEnv of
    Just ty -> pure ty
    Nothing -> die (TcError'UndefinedVariable n)

-- ==============================================
--      Creating, reading, writing MetaTvs
-- ==============================================

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
newMetaTyVarName str = newSysName (mkTyVarOccFS str)

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
        , tcLevel = ?tcLevel
        }

-- Similar to `newMetaTyVarTyAtLevel`
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1069
newMetaTyVar' :: FastString -> TcM TcTyVarMeta
newMetaTyVar' str = do
  name <- newMetaTyVarName str
  details <- newMetaDetails TyVarTv
  pure $ mkTcTyVar name details

-- TODO how to remember the origin of a metavariable?

  x' <- newMetaTyVar' x.varName.nameOcc.occNameFS
  debug'
    "tyVarToMetaTyVar"
    [ ("x", pretty' x)
    , ("x'", pretty' x')
    ]
  pure x'
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Var.hs#L1046
mkTcTyVar :: Name -> TcTyVarDetails -> TcTyVar
mkTcTyVar name details =
  TcTyVar
    { varName = name
    , varDetails = details
    }

-- | Makes a new skolem tv.
--
-- Similar to 'newSkolemTyVar'
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L765
newSkolemTyVar' :: (ITcLevel) => SkolemInfo -> Name -> TcTyVar
newSkolemTyVar' info name = mkTcTyVar name (SkolemTv info ?tcLevel)

-- | Converts a 'BoundTv' to a 'SkolemTv'
--
-- Similar to 'cloneTyVarTyVar'
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L779
newSkolemTyVar :: SkolemInfoAnon -> TcBoundVar -> TcM TcTyVar
newSkolemTyVar infoAnon tv@TcTyVar{varDetails = BoundTv{}} = do
  uniq <- newUnique
  let name = tv.varName{nameUnique = uniq}
      -- TODO should this skolemInfo contain the uniq of the original tyvar?
      -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Types/Origin.hs#L343
      skolemInfo = SkolemInfo tv.varName.nameUnique infoAnon
  pure $ newSkolemTyVar' skolemInfo name
newSkolemTyVar _ x =
  die (TcError'UnboundVariable x)

-- `metaTyVarRef` panics if a tv doesn't have a ref
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L1317

-- TODO not maybe, but Flexi and Indirect
readTv :: TcTyVarMeta -> TcM (Maybe Tau)
readTv (TcTyVar{varDetails = MetaTv{metaTvRef = ref}}) = do
  details <- readIORef ref
  pure $ case details of
    Flexi -> Nothing
    Indirect ty -> Just ty
readTv _ = pure Nothing

-- ==============================================
-- Instantiation
-- ==============================================

-- | Instantiate the topmost 'forall's of the argument type
-- with flexible type variables.
instantiate :: Sigma -> TcM Rho
instantiate (Type'ForAll tvs ty) = do
  tvs' <- forM tvs tyVarToMetaTyVar
  debug'
    "instantiate Type'ForAll"
    [ ("tvs", pretty' tvs)
    , ("ty", pretty' ty)
    , ("tvs'", pretty' tvs')
    ]
  substTy tvs (Type'Var <$> tvs') ty
instantiate ty = pure ty

-- ==============================================
-- Skolemisation
-- ==============================================

-- | Performs deep skolemisation, returning the
-- skolem constants and the skolemised type.
skolemise :: Sigma -> TcM ([TcTyVar], Rho)
-- Rule PRPOLY
skolemise (Type'ForAll tvs ty) = do
  -- TODO Do we need recursive do?
  -- Note [Keeping SkolemInfo inside a SkolemTv]
  -- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcType.hs#L579
  let info = ForAllSkol tvs
  sks1 <- mapM (newSkolemTyVar info) tvs
  (sks2, ty') <- skolemise =<< (substTy tvs (Type'Var <$> sks1) ty)
  pure (sks1 <> sks2, ty')
-- Rule PRFUN
skolemise (Type'Fun arg_ty res_ty) = do
  (sks, res_ty') <- skolemise res_ty
  pure (sks, Type'Fun arg_ty res_ty')
-- Rule PRMONO
skolemise ty = pure ([], ty)

-- | Replace the specified quantified (introduced in a 'forall') type variables by given meta type variables.
--
-- No worries about capture, because the two kinds of type variable are distinct.
substTy :: [TcBoundVar] -> [TcTypeMeta] -> TcType -> TcM TcType
substTy tvs tys ty = subst_ty (Map.fromList (tvs `zip` tys)) ty

type SubstitutionEnv = Map.Map TcBoundVar TcType

subst_ty :: SubstitutionEnv -> TcType -> TcM TcType
subst_ty env (Type'Fun arg res) =
  Type'Fun <$> (subst_ty env arg) <*> (subst_ty env res)
subst_ty env (Type'Var n@TcTyVar{varDetails = BoundTv{}}) = do
  let res = fromMaybe (Type'Var n) (Map.lookup n env)
  debug'
    "subst_ty Type'Var BoundTv"
    [ ("env", pretty' (Map.toAscList env))
    , ("res", pretty' res)
    ]
  pure res
subst_ty _ (Type'Var n@TcTyVar{varDetails = MetaTv{}}) =
  -- Just like in the paper
  pure $ Type'Var n
-- TODO should we panic if skolem?
subst_ty _ (Type'Var var@TcTyVar{}) =
  die TcError'UnboundVariable{var}
subst_ty _ (Type'Concrete tc) =
  pure $ Type'Concrete tc
subst_ty env (Type'ForAll ns rho) = do
  let ns' = Set.fromList ns
      -- TODO works correctly?
      env' = Map.withoutKeys env ns'
  -- It's not skolemisation
  -- so we can leave these variables as they are now
  Type'ForAll ns <$> (subst_ty env' rho)

-- ==============================================
-- Quantification
-- ==============================================

isMetaTv :: TcTyVar -> Bool
isMetaTv TcTyVar{varDetails = MetaTv{}} = True
isMetaTv _ = False

isFlexiTv :: TcTyVar -> TcM Bool
isFlexiTv TcTyVar{varDetails = MetaTv{metaTvRef}} =
  readIORef metaTvRef >>= \case
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

-- | Quantify over given type variables.
--
-- Should be used only in 'inferSigma'.
--
-- Note [quantifyTyVars]
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/TcMType.hs#L1677
--
-- Note [Deciding quantification]
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Solver.hs#L1138
quantify :: [TcTyVarMeta] -> Rho -> TcM Sigma
-- Quantify over the specified type variables (all flexible)
quantify tvs ty | all isMetaTv tvs = do
  -- TODO remove this check?
  tvs' <- forM tvs (\x -> (x,) <$> isFlexiTv x) >>= pure . partition snd
  case tvs' of
    (_, x : xs) ->
      die (TcError'ExpectedFlexiVariables (fst <$> (x : xs)))
    _ -> pure ()
  -- 'bind' is just a cunning way of doing the substitution
  -- TODO is it correct to use these vars in ForAll?
  vars <- mapM bind (tvs `zip` newBinders tvs ty)
  debug'
    "quantify"
    [ ("ty", pretty' ty)
    ]
  -- TODO should we zonk?
  ty' <- zonkType ty
  pure (Type'ForAll vars ty')
quantify tvs _ =
  die (TcError'ExpectedAllMetavariables tvs)

-- Avoid quantified type variables in use
newBinders :: [TcTyVarMeta] -> TcType -> [FastString]
newBinders tvs ty =
  take
    (length tvs)
    (allBinders \\ ((.varName.nameOcc.occNameFS) <$> tyVarBndrs ty))

bind :: (TcTyVarMeta, FastString) -> TcM TcTyVar
bind (tv, name) = do
  -- TODO use info from tv?
  name' <- newSysName (mkTyVarOccFS name)
  -- TODO this should be boundtv, not flexi
  -- Why?
  -- TODO is tcLevel correct?
  let var =
        TcTyVar
          { varName = name'
          , varDetails = BoundTv{tcLevel = ?tcLevel}
          }
  -- unifyVar
  -- unifyVar Nothing tv (Type'Var var)
  -- writeTv tv (Type'Var var)
  pure var

tyVarBndrs :: Rho -> [TcTyVar]
-- Get all the binders used in ForAlls in the type, so that
-- when quantifying an outer for-all we can avoid these inner ones
tyVarBndrs ty = nub (bndrs ty)
 where
  bndrs = \case
    Type'ForAll vars body -> vars <> bndrs body
    Type'Fun arg res -> bndrs arg <> bndrs res
    _ -> []

-- ==============================================
-- Zonking
-- Eliminate any substitutions in the type
-- ==============================================

-- TODO backsubstitution
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Zonk/Type.hs#L925

zonkType :: TcType -> TcM TcType
zonkType (Type'ForAll ns ty) = do
  ty' <- zonkType ty
  pure (Type'ForAll ns ty')
zonkType (Type'Fun arg res) = do
  debug'
    "zonkType"
    [ ("arg", pretty' arg)
    , ("arg", prettyVerbose arg)
    , ("res", pretty' res)
    , ("res", prettyVerbose res)
    ]
  arg' <- zonkType arg
  res' <- zonkType res
  pure (Type'Fun arg' res')
zonkType (Type'Concrete tc) = pure (Type'Concrete tc)
zonkType v@(Type'Var var) =
  case var of
    TcTyVar{varDetails = MetaTv{}} ->
      do
        mb_ty <- readTv var
        case mb_ty of
          -- TODO why does zonking keep metavariables with flexi types?
          Nothing -> pure v
          Just ty -> do
            ty' <- zonkType ty
            -- "Short out" multiple hops
            writeIORef metaTvRef (Indirect ty)
            pure ty'
    _ -> pure (Type'Var var)

-- ==============================================
--      Unification
-- ==============================================

badType :: Tau -> Bool
-- Tells which types should never be encountered during unification
badType (Type'Var TcTyVar{varDetails = BoundTv{}}) = True
badType _ = False

-- | Extends the substitution by side effect (p. 43)
--
-- It's similar to `unifyType` in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L2011
--
-- See Note [Unification preconditions]:
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Tc/Utils/Unify.hs#L2589
unify :: Maybe TypedThing -> Tau -> Tau -> TcM ()
-- Invariant:
-- The first type is "expected",
-- the second one is "actual"
unify thing ty1 ty2
  | badType ty1 || badType ty2 -- Compiler error
    =
      do
        debug'
          "unify bound types"
          [ ("?varEnv", prettyVerbose (Map.toAscList ?varEnv))
          ]
        die (TcError'UnifyingBoundTypes ty1 ty2 thing)
unify
  _thing
  (Type'Var TcTyVar{varDetails = MetaTv{metaTvRef = tv1}})
  (Type'Var TcTyVar{varDetails = MetaTv{metaTvRef = tv2}})
    | tv1 == tv2 = pure ()
unify thing (Type'Var tv@TcTyVar{varDetails = MetaTv{}}) ty =
  unifyVar thing tv ty False
unify thing ty (Type'Var tv@TcTyVar{varDetails = MetaTv{}}) =
  -- We have to let unifyVar know that we swapped "expected" and "actual"
  unifyVar thing tv ty True
-- TODO is equality defined correctly?
unify _thing (Type'Var tv1@TcTyVar{}) (Type'Var tv2@TcTyVar{})
  | tv1 == tv2 = pure ()
unify _thing (Type'Fun arg1 res1) (Type'Fun arg2 res2) = do
  -- TODO should we inspect thing?
  unify Nothing arg1 arg2
  unify Nothing res1 res2
unify _thing (Type'Concrete tc1) (Type'Concrete tc2)
  | tc1 == tc2 = pure ()
unify thing ty1 ty2 =
  die (TcError'CannotUnify{ty1 = ty1, ty2 = ty2, thing})

mkCEqCan :: Maybe TypedThing -> TcTyVar -> Tau -> Bool -> Ct
mkCEqCan thing tv ty swapped =
  CEqCan
    EqCt
      { eq_ev =
          CtWanted
            WantedCt
              { ctev_loc =
                  CtLoc
                    { ctl_origin =
                        TypeEqOrigin
                          { uo_actual = if swapped then ty else Type'Var tv
                          , uo_expected = if swapped then Type'Var tv else ty
                          , uo_thing = thing
                          }
                    , ctl_env = Nothing
                    }
              }
      , eq_lhs = tv
      , eq_rhs = ty
      }

emitCEqCan :: Maybe TypedThing -> TcTyVar -> Tau -> Bool -> TcM ()
emitCEqCan thing tv ty swapped = do
  constraintsCur <- readIORef ?constraints
  let constraint = mkCEqCan thing tv ty swapped
      constraints' = constraints{wc_simple = Bag [constraint] <> constraints.wc_simple}
  -- debug "unifyVar" [pretty constraint]
  writeTcRef ?constraints constraints'

-- | Unify a variable with a type.
--
-- The 'Maybe TypedThing' may provide evidence for this equality
unifyVar :: Maybe TypedThing -> TcTyVar -> Tau -> Bool -> TcM ()
-- Invariant: tv is a flexible type variable
-- Check whether tv is bound
unifyVar thing tv ty swapped | isMetaTv tv = do
  emitCEqCan thing tv ty swapped
-- mb_ty1 <- readTv tv
-- case mb_ty1 of
--   Just ty1 -> unify thing ty1 ty
--   Nothing -> unifyUnboundVar thing tv ty
unifyVar _thing _tv _ty _ = pure ()

-----------------------------------------

unifyFun :: Maybe TypedThing -> Rho -> TcM (Sigma, Rho)
-- unifies 'fun' with '(arg -> res)'
unifyFun _thing (Type'Fun arg res) = pure (arg, res)
unifyFun thing tau = do
  -- TODO use better names?
  arg_ty <- Type'Var <$> newMetaTyVar' "a"
  res_ty <- Type'Var <$> newMetaTyVar' "b"
  unify thing tau (Type'Fun arg_ty res_ty)
  pure (arg_ty, res_ty)