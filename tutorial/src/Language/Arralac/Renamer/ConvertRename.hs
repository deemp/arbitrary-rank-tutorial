module Language.Arralac.Renamer.ConvertRename where

import Control.Monad (forM)
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Base (when)
import GHC.Stack (HasCallStack)
import Language.Arralac.Parser.Abs qualified as Abs
import Language.Arralac.Renamer.Error
import Language.Arralac.Renamer.Types
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Syntax.TTG.Type
import Language.Arralac.Utils.Types
import Language.Arralac.Utils.Types.Pass
import Language.Arralac.Utils.Unique (Unique)
import Language.Arralac.Utils.Unique.Supply (IUniqueSupply, newUnique)

-- ===================================
-- [Convert and rename the parser AST]
-- ===================================

convertRenameAbs :: (HasCallStack, ICurrentFilePath, IDebug, IUniqueSupply, ConvertRename a) => a -> ConvertRenameTo a
convertRenameAbs a = do
  let
    ?termVarScope = Map.empty
    ?tyVarScope = Map.empty
    -- TODO put existing types here?
    ?tyConcreteScope = Map.empty
    ?letOccursCheckInfo = Nothing
  convertRename a

class ConvertRename a where
  type ConvertRenameTo a
  convertRename :: (IRnConstraints) => a -> (ConvertRenameTo a)

instance ConvertRename Abs.Program where
  type ConvertRenameTo Abs.Program = IO (SynTerm CompRn)
  convertRename (Abs.Program _ program) =
    convertRename program

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

runWithScope :: NameSpace -> Scope -> RnM a -> RnM a
runWithScope ns scope act =
  case ns of
    NameSpace'TermVar -> let ?termVarScope = scope in act
    NameSpace'TypeVar -> let ?tyVarScope = scope in act
    NameSpace'TypeConcrete -> let ?tyConcreteScope = scope in act

selectScope :: (IRnScopes) => NameSpace -> Scope
selectScope = \case
  NameSpace'TermVar -> ?termVarScope
  NameSpace'TypeVar -> ?tyVarScope
  NameSpace'TypeConcrete -> ?tyConcreteScope

withNameInScope :: NameSpace -> Name -> RnM a -> RnM a
withNameInScope ns name act = do
  let scope = Map.insert name.nameOcc.occNameFS name.nameUnique (selectScope ns)
  runWithScope ns scope act

letOccursCheck :: Name -> RnM ()
letOccursCheck name = do
  case ?letOccursCheckInfo of
    Just info ->
      when (name.nameOcc.occNameFS == info.letLhs.nameOcc.occNameFS) $
        dieRn
          RnError'LetOccursCheckFailed
            { letOccursCheckInfo = info
            , letLhsOcc = name
            }
    _ -> pure ()

instance ConvertRename Abs.Exp where
  type ConvertRenameTo Abs.Exp = IO (SynTerm CompRn)
  convertRename = \case
    Abs.ExpVar _pos var -> do
      var' <- convertRename var False
      letOccursCheck var'
      pure $ SynTerm'Var () var'
    Abs.ExpInt pos val ->
      pure $ SynTerm'Lit (convertPositionToSrcSpan pos) (SynLit'Num val)
    Abs.ExpString pos val ->
      pure $ SynTerm'Lit (convertPositionToSrcSpan pos) (SynLit'Str (T.pack val))
    Abs.ExpBool pos val -> do
      let val' = case val of
            Abs.BoolTrue _ -> True
            Abs.BoolFalse _ -> False
      pure $ SynTerm'Lit (convertPositionToSrcSpan pos) (SynLit'Bool val')
    Abs.ExpCon _ (Abs.Con pos (Abs.NameUpperCase name)) ->
      pure $ SynTerm'Lit (convertPositionToSrcSpan pos) (SynLit'Con name)
    Abs.ExpApp pos term1 term2 -> do
      term1' <- convertRename term1
      term2' <- convertRename term2
      pure $ SynTerm'App (convertPositionToSrcSpan pos) term1' term2'
    Abs.ExpAbs pos var term -> do
      var' <- convertRename var True
      term' <- withNameInScope NameSpace'TermVar var' (convertRename term)
      pure $ SynTerm'Lam (convertPositionToSrcSpan pos) var' term'
    Abs.ExpAbsAnno pos var ty term -> do
      var' <- convertRename var True
      ty' <- convertRename ty
      term' <- withNameInScope NameSpace'TermVar var' (convertRename term)
      pure $ SynTerm'ALam (convertPositionToSrcSpan pos) var' ty' term'
    Abs.ExpLet pos var term1 term2 -> do
      var' <- convertRename var True
      -- TODO should the let-expression be recursive
      -- and the var be brought into scope of the assigned term?
      let pos' = convertPositionToSrcSpan pos
      term1' <- do
        let ?letOccursCheckInfo =
              Just
                LetOccursCheckInfo
                  { letSrcSpan = pos'
                  , letLhs = var'
                  }
        convertRename term1
      term2' <- withNameInScope NameSpace'TermVar var' (convertRename term2)
      pure $ SynTerm'Let pos' var' term1' term2'
    Abs.ExpAnno pos term ty -> do
      term' <- convertRename term
      ty' <- convertRename ty
      pure $ SynTerm'Ann (convertPositionToSrcSpan pos) term' ty'

getVarId :: (IRnScopes) => NameSpace -> NameFs -> Maybe Unique
getVarId ns k = Map.lookup k (selectScope ns)

getExistingOrNewUnique :: NameSpace -> NameFs -> RnM Unique
getExistingOrNewUnique ns name = maybe newUnique pure (getVarId ns name)

instance ConvertRename Abs.Var where
  type ConvertRenameTo Abs.Var = Bool -> IO Name
  convertRename (Abs.Var pos (Abs.NameLowerCase name)) needUnique = do
    let ns = NameSpace'TermVar
    nameUnique <-
      if needUnique
        then newUnique
        else getExistingOrNewUnique ns name
    pure $
      Name
        { nameOcc =
            OccName
              { occNameSpace = ns
              , occNameFS = name
              }
        , nameUnique
        , nameLoc = (convertPositionToSrcSpan pos)
        }

instance ConvertRename Abs.TypeVariable where
  type ConvertRenameTo Abs.TypeVariable = IO Name
  convertRename (Abs.TypeVariableName pos (Abs.NameLowerCase name)) = do
    -- Each type variable in a `forall`
    -- must have a globally unique identifier.
    nameUnique <- newUnique
    pure $
      Name
        { nameOcc =
            OccName
              { occNameSpace = NameSpace'TypeVar
              , occNameFS = name
              }
        , nameUnique
        , nameLoc = (convertPositionToSrcSpan pos)
        }

parseTypeConcrete :: NameFs -> TypeConcrete
parseTypeConcrete name = do
  if
    | name == typeConcreteName TypeConcrete'Bool -> TypeConcrete'Bool
    | name == typeConcreteName TypeConcrete'String -> TypeConcrete'String
    | name == typeConcreteName TypeConcrete'Int -> TypeConcrete'Int
    | otherwise -> TypeConcrete'Con name

withNamesInScope :: NameSpace -> [Name] -> RnM a -> RnM a
withNamesInScope ns names act = do
  -- Map.union prefers the first argument
  -- and we need to add new names to the scope
  -- to implement shadowing
  let scope =
        Map.union
          ( Map.fromList
              ((\name -> (name.nameOcc.occNameFS, name.nameUnique)) <$> names)
          )
          $ selectScope ns
  runWithScope ns scope act

getExistingUnique :: NameSpace -> Abs.BNFC'Position -> NameFs -> RnM Unique
getExistingUnique ns pos name =
  case getVarId ns name of
    Nothing ->
      dieRn
        RnError'UnboundTypeVariable
          { name
          , srcSpan = convertPositionToSrcSpan pos
          }
    Just u -> pure u

instance ConvertRename Abs.Type where
  type ConvertRenameTo Abs.Type = IO (SynType CompRn)
  convertRename = \case
    -- TODO not a variable
    Abs.TypeConcrete pos (Abs.NameUpperCase name) -> do
      -- TODO should all mentions of a type have the same uniques?
      let ns = NameSpace'TypeConcrete
          srcSpan = convertPositionToSrcSpan pos
          concreteType = parseTypeConcrete name
      nameUnique <- getExistingOrNewUnique ns name
      pure $
        SynType'Concrete
          ()
          Concrete
            { concreteType
            , concreteName =
                Name
                  { nameOcc =
                      OccName
                        { occNameSpace = ns
                        , occNameFS = name
                        }
                  , nameUnique
                  , nameLoc = srcSpan
                  }
            }
    Abs.TypeVariable pos (Abs.NameLowerCase name) -> do
      let ns = NameSpace'TypeVar
      -- Each type variable in the type body must be bound.
      nameUnique <- getExistingUnique ns pos name
      pure $
        SynType'Var
          ()
          ( Name
              { nameOcc =
                  OccName
                    { occNameSpace = ns
                    , occNameFS = name
                    }
              , nameUnique
              , nameLoc = (convertPositionToSrcSpan pos)
              }
          )
    Abs.TypeForall pos tys ty -> do
      when (tys == []) $
        dieRn
          RnError'ForallBindsNoTvs
            { srcSpan = convertPositionToSrcSpan pos
            }
      tys' <- forM tys convertRename
      ty' <- withNamesInScope NameSpace'TypeVar tys' (convertRename ty)
      pure $ SynType'ForAll (convertPositionToSrcSpan pos) tys' ty'
    Abs.TypeFunc pos ty1 ty2 ->
      SynType'Fun (convertPositionToSrcSpan pos)
        <$> (convertRename ty1)
        <*> (convertRename ty2)
