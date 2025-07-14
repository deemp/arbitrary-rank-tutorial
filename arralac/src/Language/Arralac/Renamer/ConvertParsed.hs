module Language.Arralac.Renamer.ConvertParsed where

import Control.Monad (forM)
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Base (when)
import Language.Arralac.Parser.Abs qualified as Abs
import Language.Arralac.Prelude.Pass
import Language.Arralac.Prelude.Types
import Language.Arralac.Prelude.Unique (Unique)
import Language.Arralac.Prelude.Unique.Supply (newUnique)
import Language.Arralac.Renamer.Error
import Language.Arralac.Renamer.Types
import Language.Arralac.Syntax.Local.Extension.Rn ()
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.SynLit
import Language.Arralac.Syntax.Local.SynTerm.Rn ()
import Language.Arralac.Syntax.Local.SynTermVar.Rn
import Language.Arralac.Syntax.Local.SynType.Rn ()
import Language.Arralac.Syntax.Local.Type
import Language.Arralac.Syntax.TTG.SynTerm
import Language.Arralac.Syntax.TTG.SynType
import Language.Arralac.Syntax.TTG.Type

-- ===================================
-- [Convert and rename the parser AST]
-- ===================================

class ConvertParsed a where
  type ConvertParsedTo a
  convertParsed :: (CtxRnConstraints) => a -> (ConvertParsedTo a)

instance ConvertParsed Abs.Program where
  type ConvertParsedTo Abs.Program = IO (SynTerm CompRn)
  convertParsed (Abs.Program _ program) =
    convertParsed program

convertPositionToSrcSpan :: (CtxInputFilePath) => Abs.BNFC'Position -> SrcSpan
convertPositionToSrcSpan = \case
  Just ((sLine, sCol), (eLine, eCol)) ->
    RealSrcSpan
      RealSrcSpan'
        { srcSpanFile = ?inputFilePath
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
    NameSpace'TyVar -> let ?tyVarScope = scope in act
    NameSpace'TyConcrete -> let ?tyConcreteScope = scope in act

selectScope :: (CtxRnScopes) => NameSpace -> Scope
selectScope = \case
  NameSpace'TermVar -> ?termVarScope
  NameSpace'TyVar -> ?tyVarScope
  NameSpace'TyConcrete -> ?tyConcreteScope

withVarInScope :: NameSpace -> RnVar -> RnM a -> RnM a
withVarInScope ns var act = do
  let scope =
        Map.insert
          var.varName.nameOcc.occNameFS
          var.varName.nameUnique
          (selectScope ns)
  runWithScope ns scope act

letOccursCheck :: RnVar -> RnM ()
letOccursCheck var = do
  case ?letOccursCheckInfo of
    Just info ->
      when
        ( var.varName.nameOcc.occNameFS
            == info.letLhs.varName.nameOcc.occNameFS
        )
        $ dieRn
          RnError'LetOccursCheckFailed
            { letOccursCheckInfo = info
            , letLhsOcc = var
            }
    _ -> pure ()

instance ConvertParsed Abs.Exp where
  type ConvertParsedTo Abs.Exp = IO (SynTerm CompRn)
  convertParsed = \case
    Abs.ExpVar _pos var -> do
      var' <- convertParsed var False
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
      term1' <- convertParsed term1
      term2' <- convertParsed term2
      pure $ SynTerm'App (convertPositionToSrcSpan pos) term1' term2'
    Abs.ExpAbs pos var term -> do
      var' <- convertParsed var True
      term' <- withVarInScope NameSpace'TermVar var' (convertParsed term)
      pure $ SynTerm'Lam (convertPositionToSrcSpan pos) var' term'
    Abs.ExpAbsAnno pos var ty term -> do
      var' <- convertParsed var True
      ty' <- convertParsed ty
      term' <- withVarInScope NameSpace'TermVar var' (convertParsed term)
      pure $ SynTerm'ALam (convertPositionToSrcSpan pos) var' ty' term'
    Abs.ExpLet pos var term1 term2 -> do
      var' <- convertParsed var True
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
        convertParsed term1
      term2' <- withVarInScope NameSpace'TermVar var' (convertParsed term2)
      pure $ SynTerm'Let pos' var' term1' term2'
    Abs.ExpAnno pos term ty -> do
      term' <- convertParsed term
      ty' <- convertParsed ty
      pure $ SynTerm'Ann (convertPositionToSrcSpan pos) term' ty'

lookupVarUnique :: (CtxRnConstraints) => NameSpace -> NameFs -> Maybe Unique
lookupVarUnique ns k = Map.lookup k (selectScope ns)

getExistingOrNewUnique :: NameSpace -> NameFs -> RnM Unique
getExistingOrNewUnique ns name = maybe newUnique pure (lookupVarUnique ns name)

getExistingUnique :: NameSpace -> Abs.BNFC'Position -> NameFs -> RnM Unique
getExistingUnique ns pos name =
  case lookupVarUnique ns name of
    Nothing ->
      dieRn $
        case ns of
          NameSpace'TermVar ->
            RnError'UnboundTermVariable
              { name
              , srcSpan = convertPositionToSrcSpan pos
              }
          _ ->
            RnError'UnboundTypeVariable
              { name
              , srcSpan = convertPositionToSrcSpan pos
              }
    Just u -> pure u

instance ConvertParsed Abs.Var where
  type ConvertParsedTo Abs.Var = Bool -> IO RnVar
  convertParsed (Abs.Var pos (Abs.NameLowerCase name)) needUnique = do
    let ns = NameSpace'TermVar
    nameUnique <-
      if
        | needUnique -> newUnique
        | otherwise -> getExistingUnique NameSpace'TermVar pos name
    pure $
      RnVar
        Name
          { nameOcc =
              OccName
                { occNameSpace = ns
                , occNameFS = name
                }
          , nameUnique
          , nameLoc = (convertPositionToSrcSpan pos)
          }

instance ConvertParsed Abs.TypeVariable where
  type ConvertParsedTo Abs.TypeVariable = IO Name
  convertParsed (Abs.TypeVariableName pos (Abs.NameLowerCase name)) = do
    -- Each type variable in a `forall`
    -- must have a globally unique identifier.
    nameUnique <- newUnique
    pure $
      Name
        { nameOcc =
            OccName
              { occNameSpace = NameSpace'TyVar
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

instance ConvertParsed Abs.Type where
  type ConvertParsedTo Abs.Type = IO (SynType CompRn)
  convertParsed = \case
    -- TODO not a variable
    Abs.TypeConcrete pos (Abs.NameUpperCase name) -> do
      -- TODO should all mentions of a type have the same uniques?
      let ns = NameSpace'TyConcrete
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
      let ns = NameSpace'TyVar
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
      tys' <- forM tys convertParsed
      ty' <- withNamesInScope NameSpace'TyVar tys' (convertParsed ty)
      pure $ SynType'ForAll (convertPositionToSrcSpan pos) tys' ty'
    Abs.TypeFunc pos ty1 ty2 ->
      SynType'Fun (convertPositionToSrcSpan pos)
        <$> (convertParsed ty1)
        <*> (convertParsed ty2)
