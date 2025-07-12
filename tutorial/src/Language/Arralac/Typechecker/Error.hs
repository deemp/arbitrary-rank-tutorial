{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Typechecker.Error where

import Control.Exception (Exception, throw)
import Data.IORef (IORef, readIORef, writeIORef)
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.Local.Var.Tc
import Language.Arralac.Syntax.TTG.SynTerm (SynTerm (..))
import Language.Arralac.Typechecker.Constraints
import Language.Arralac.Utils.Pretty
import Prettyprinter (Doc)

-- =====================
-- [Type checker errors]
-- =====================

type CtxTcErrorPropagated = (?tcErrorPropagated :: IORef (Maybe TcError))

data TcError
  = TcError'UndefinedVariable {varName :: Name}
  | TcError'UnboundVariable {var :: TcTyVar}
  | TcError'UnexpectedType {expected :: TcType, actual :: TcType, thing :: Maybe TypedThing}
  | -- TODO specify which type(s) are bound
    TcError'UnifyingBoundTypes {ty1 :: TcType, ty2 :: TcType, thing :: Maybe TypedThing}
  | TcError'ExpectedFlexiVariables {tvs :: [TcTyVar]}
  | TcError'UnknownConcreteType {name :: Name}
  | TcError'ExpectedAllMetavariables {tvs :: [TcTyVar]}
  | TcError'CannotUnify {ty1 :: TcType, ty2 :: TcType, thing :: Maybe TypedThing}

-- Capture current callstack in GADT
-- https://maksbotan.github.io/posts/2021-01-20-callstacks.html
data TcErrorWithCallStack where
  TcErrorWithCallStack :: (HasCallStack) => TcError -> TcErrorWithCallStack

dieTc :: (CtxTcErrorPropagated, HasCallStack) => TcError -> IO a -- Fail unconditionally
dieTc tcErrorCurrent = do
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

withTcError :: (CtxTcErrorPropagated) => TcError -> ((CtxTcErrorPropagated) => IO a) -> IO a
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

instance Exception TcError

instance Exception TcErrorWithCallStack

getThingStart :: TypedThing -> SrcSpan
getThingStart (TypedThing'SynTermRn thing) =
  case thing of
    SynTerm'Var _ var -> var.nameLoc
    SynTerm'Lit anno _ -> anno
    SynTerm'App anno _ _ -> anno
    SynTerm'Lam anno _ _ -> anno
    SynTerm'ALam anno _ _ _ -> anno
    SynTerm'Let anno _ _ _ -> anno
    SynTerm'Ann anno _ _ -> anno

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
      vsep' $
        [ "Expected the type:"
        , prettyIndent expected
        , "but got the type:"
        , prettyIndent actual
        ]
          <> prettyThingLocation thing
    TcError'UnifyingBoundTypes{ty1, ty2, thing} ->
      vsep' $
        [ "Trying to unify type:"
        , prettyIndent ty1
        , "with type:"
        , prettyIndent ty2
        ]
          <> prettyThingLocation thing
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
      vsep' $
        [ "Cannot unify type:"
        , prettyIndent ty1
        , "with type:"
        , prettyIndent ty2
        ]
          <> prettyThingLocation thing
   where
    prettyThingLocation :: Maybe TypedThing -> [Doc ann]
    prettyThingLocation = \case
      Nothing -> mempty
      Just thing ->
        [ "in the expression:"
        , prettyIndent thing
        ]
          <> prettyDefinedAt thing
    prettyDefinedAt :: (CtxPrettyVerbosity) => TypedThing -> [Doc ann]
    prettyDefinedAt thing =
      [ "defined at:"
      , prettyIndent (getThingStart thing)
      ]

instance Pretty' TcErrorWithCallStack where
  pretty' (TcErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]
