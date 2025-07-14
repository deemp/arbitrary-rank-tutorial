{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Typechecker.Error where

import Control.Exception (Exception, throw)
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Syntax.Local.Extension.Rn ()
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.TyVar.Rn
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynTerm (SynTerm (..))
import Language.Arralac.Typechecker.Constraints
import Prettyprinter (Doc)

-- ====================
-- [Typechecker errors]
-- ====================

type CtxTcErrorPropagated = (?tcErrorPropagated :: Maybe TcError)

data TcError
  = TcError'UndefinedVariable {rnVar :: RnVar}
  | TcError'UnboundVariable {var :: TcTyVar}
  | TcError'UnexpectedType {expected :: TcType, actual :: TcType, thing :: Maybe TypedThing}
  | -- TODO specify which type(s) are bound
    TcError'UnifyingBoundTypes {ty1 :: TcType, ty2 :: TcType, thing :: Maybe TypedThing}
  | TcError'ExpectedFlexiVariables {tvs :: [TcTyVar]}
  | TcError'UnknownConcreteType {name :: Name}
  | TcError'ExpectedAllMetavariables {tvs :: [TcTyVar]}
  | TcError'CannotUnify {ty1 :: TcType, ty2 :: TcType, thing :: Maybe TypedThing}

-- | A typechecker error.
--
-- Capture current callstack in GADT.
-- https://maksbotan.github.io/posts/2021-01-20-callstacks.html
data TcErrorWithCallStack where
  TcErrorWithCallStack :: (HasCallStack) => TcError -> TcErrorWithCallStack

dieTc :: (CtxTcErrorPropagated, HasCallStack) => TcError -> IO a -- Fail unconditionally
dieTc tcErrorCurrent = do
  -- TODO Currently, a newer error can not overwrite the propagated error.
  -- We need this behavior to propagate actual vs expected type errors.
  -- If there's a mismatch during unification, we can immediately report.
  -- Should we choose the error for each combination of types of errors (new, propagated)?
  let error' =
        case ?tcErrorPropagated of
          Nothing -> tcErrorCurrent
          Just err -> err
  throw (TcErrorWithCallStack error')

withTcError :: (CtxTcErrorPropagated) => TcError -> ((CtxTcErrorPropagated) => IO a) -> IO a
withTcError err tcAction = do
  case ?tcErrorPropagated of
    Nothing -> do
      let ?tcErrorPropagated = Just err
      do
        tcAction
    Just _ -> do
      tcAction

instance Exception TcError

instance Exception TcErrorWithCallStack

getThingStart :: TypedThing -> SrcSpan
getThingStart (TypedThing'SynTermRn thing) =
  case thing of
    SynTerm'Var _ var -> var.varName.nameLoc
    SynTerm'Lit anno _ -> anno
    SynTerm'App anno _ _ -> anno
    SynTerm'Lam anno _ _ -> anno
    SynTerm'ALam anno _ _ _ -> anno
    SynTerm'Let anno _ _ _ -> anno
    SynTerm'Ann anno _ _ -> anno

instance Pretty' TcError where
  pretty' err = case err of
    TcError'UndefinedVariable{} ->
      vsep'
        [ "Variable not in scope:"
        , prettyIndent err.rnVar.varName
        , "at:"
        , prettyIndent err.rnVar.varName.nameLoc
        ]
    TcError'UnboundVariable{} ->
      vsep'
        [ "Expected a bound variable, but got:"
        , prettyIndent err.var
        , "at:"
        , prettyIndent err.var.varName.nameLoc
        ]
    TcError'UnexpectedType{} ->
      vsep' $
        [ "Expected the type:"
        , prettyIndent err.expected
        , "but got the type:"
        , prettyIndent err.actual
        ]
          <> prettyThingLocation err.thing
    TcError'UnifyingBoundTypes{} ->
      vsep' $
        [ "Trying to unify type:"
        , prettyIndent err.ty1
        , "with type:"
        , prettyIndent err.ty2
        ]
          <> prettyThingLocation err.thing
    TcError'ExpectedFlexiVariables{} ->
      vsep'
        [ "Expected all variables to be Flexi, but these are not:"
        , prettyIndent err.tvs
        ]
    TcError'UnknownConcreteType{} ->
      vsep'
        [ "Unknown concrete type:"
        , prettyIndent err.name
        , "at:"
        , prettyIndent err.name.nameLoc
        ]
    TcError'ExpectedAllMetavariables{} ->
      vsep'
        [ "Expected all variables to be metavariables, but got:"
        , prettyIndent err.tvs
        ]
    TcError'CannotUnify{} ->
      vsep' $
        [ "Cannot unify type:"
        , prettyIndent err.ty1
        , "with type:"
        , prettyIndent err.ty2
        ]
          <> prettyThingLocation err.thing
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
