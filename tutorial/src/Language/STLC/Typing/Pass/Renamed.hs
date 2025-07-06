{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC.Typing.Pass.Renamed where

import Control.Exception (Exception, throw)
import Control.Monad (forM)
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.IORef (readIORef, writeIORef)
import GHC.Stack (HasCallStack, callStack)
import Language.STLC.Common
import Language.STLC.Syntax.Abs qualified as Abs
import Language.STLC.Syntax.Par
import Language.STLC.Typing.Jones2007.BasicTypes as BT
import qualified Data.Text.IO as T
import GHC.Exception (prettyCallStack)

data ParseErrorWithCallStack where
  ParseErrorWithCallStack :: (HasCallStack) => String -> ParseErrorWithCallStack

-- TODO add index because the parse type isn't enough to differentiate
-- TODO add built-in types to the scope
-- TODO Should we have a separate scope for terms and types?
-- TODO Forbid shadowing?
type IConvertRename = (IUniqueSupply, IScope, ICurrentFilePath)

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

convertProgram :: (IConvertRename) => Abs.Program -> IO (BT.SynTerm BT.CompRn)
convertProgram (Abs.Program _ program) = convertAbsToBT program

type RnM a = (BT.IUniqueSupply, BT.IScope, BT.ICurrentFilePath, BT.IDebug) => IO a

parseInputText :: T.Text -> RnM (BT.SynTerm BT.CompRn)
parseInputText input = do
  let
    parsed = parseWith pProgram input
  case parsed of
    -- TODO throw better and handle where necessary
    Left err -> throw (ParseErrorWithCallStack err)
    Right prog -> convertProgram prog

parseInput :: Either String T.Text -> RnM (BT.SynTerm BT.CompRn)
parseInput input = do
  input' <- either T.readFile pure input
  parseInputText input'

parseFile :: String -> RnM (BT.SynTerm BT.CompRn)
parseFile filename = parseInput (Left filename)

parseString :: T.Text -> RnM (BT.SynTerm BT.CompRn)
parseString input = parseInput (Right input)

-- TODO use different annotations in different phases
-- TODO throw exceptions when encounter conversion errors
-- e.g. forall without variables, shadowing

instance Show ParseErrorWithCallStack where
  show (ParseErrorWithCallStack err) = prettyCallStack callStack <> "\n\n" <> err

instance Exception ParseErrorWithCallStack

class ConvertAbsToBT a where
  type To a
  convertAbsToBT :: (HasCallStack, IConvertRename) => a -> (To a)

convertPositionToSrcSpan :: (ICurrentFilePath) => Abs.BNFC'Position -> SrcSpan
convertPositionToSrcSpan = \case
  Just ((sLine, sCol), (eLine, eCol)) ->
    RealSrcSpan
      RealSrcSpan'
        { srcSpanFile = ?currentFilePath
        , srcSpanSLine = sLine - 1
        , srcSpanSCol = sCol - 1
        , srcSpanELine = eLine - 1
        , srcSpanECol = eCol - 1
        }
  Nothing ->
    UnhelpfulSpan UnhelpfulNoLocationInfo

instance ConvertAbsToBT Abs.Exp where
  type To Abs.Exp = IO (SynTerm CompRn)
  convertAbsToBT = \case
    Abs.ExpVar _pos var -> do
      var' <- convertAbsToBT var False
      pure $ SynTerm'Var () var'
    Abs.ExpInt pos val ->
      pure $ SynTerm'Lit (convertPositionToSrcSpan pos) (SynLit'Num val)
    Abs.ExpString pos val ->
      pure $ SynTerm'Lit (convertPositionToSrcSpan pos) (SynLit'Str (T.pack val))
    Abs.ExpApp pos term1 term2 -> do
      term1' <- convertAbsToBT term1
      term2' <- convertAbsToBT term2
      pure $ SynTerm'App (convertPositionToSrcSpan pos) term1' term2'
    Abs.ExpAbs pos var term -> do
      var' <- convertAbsToBT var True
      term' <- withNameInScope var' (convertAbsToBT term)
      pure $ SynTerm'Lam (convertPositionToSrcSpan pos) var' term'
    Abs.ExpAbsAnno pos var ty term -> do
      var' <- convertAbsToBT var True
      ty' <- convertAbsToBT ty
      term' <- withNameInScope var' (convertAbsToBT term)
      pure $ SynTerm'ALam (convertPositionToSrcSpan pos) var' ty' term'
    Abs.ExpLet pos var term1 term2 -> do
      var' <- convertAbsToBT var True
      -- TODO should the let-expression be recursive
      -- and the var be brought into scope of the assigned term?
      term1' <- convertAbsToBT term1
      term2' <- withNameInScope var' (convertAbsToBT term2)
      pure $ SynTerm'Let (convertPositionToSrcSpan pos) var' term1' term2'
    Abs.ExpAnno pos term ty -> do
      term' <- convertAbsToBT term
      ty' <- convertAbsToBT ty
      pure $ SynTerm'Ann (convertPositionToSrcSpan pos) term' ty'

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
        , nameLoc = (convertPositionToSrcSpan pos)
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
        , nameLoc = (convertPositionToSrcSpan pos)
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
            , nameLoc = (convertPositionToSrcSpan pos)
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
              , nameLoc = (convertPositionToSrcSpan pos)
              }
          )
    -- TODO throw if forall introduces zero type variables?
    -- or, prohibit this at parser level?
    Abs.TypeForall pos tys ty -> do
      -- TODO report
      -- No variables bound at
      tys' <- forM tys (\x -> convertAbsToBT x)
      ty' <- withNamesInScope tys' (convertAbsToBT ty)
      pure $ SynType'ForAll (convertPositionToSrcSpan pos) tys' ty'
    Abs.TypeFunc pos ty1 ty2 ->
      SynType'Fun (convertPositionToSrcSpan pos)
        <$> (convertAbsToBT ty1)
        <*> (convertAbsToBT ty2)
