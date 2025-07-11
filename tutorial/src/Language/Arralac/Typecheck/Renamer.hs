module Language.Arralac.Typecheck.Renamer where

import Control.Exception (Exception, throw)
import Control.Monad (forM)
import Data.Char (isDigit)
import Data.Function ((&))
import Data.IORef (readIORef, writeIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Base (when)
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Language.Arralac.Parse.Arralac.Lex (Token)
import Language.Arralac.Parse.Arralac.Par (myLexer, pProgram)
import Language.Arralac.Parser.Abs (BNFC'Position)
import Language.Arralac.Parser.Abs qualified as Abs
import Language.Arralac.Typecheck.Jones2007.BasicTypes as BT
import Prettyprinter (indent)

-- | A name.
type NameFs = FastString

-- | Variables scope.
--
-- Variable names and their ids.
type Scope = Map NameFs Int

-- | Current term variables scope.
--
-- Visible term variable names and their ids.
type ITermVarScope = (?termVarScope :: Scope)

-- | Current type variables scope.
--
-- Visible type variable names and their ids.
type ITyVarScope = (?tyVarScope :: Scope)

-- | Built-in types scope.
--
-- Built-in type names and their ids.
type ITyConcreteScope = (?tyConcreteScope :: Scope)

-- TODO add index because the parse type isn't enough to differentiate
-- TODO add built-in types to the scope
-- TODO Forbid shadowing?

type IRnConstraints = (HasCallStack, BT.IUniqueSupply, ITermVarScope, ITyVarScope, ITyConcreteScope, BT.ICurrentFilePath, BT.IDebug)

type RnM a = (IRnConstraints) => IO a

-- | Similar to `genSym` in GHC.
--
-- https://github.com/ghc/ghc/blob/ed38c09bd89307a7d3f219e1965a0d9743d0ca73/compiler/GHC/Types/Unique/Supply.hs#L257
newUnique :: (IUniqueSupply) => IO Int
newUnique = do
  r <- readIORef ?uniqueSupply
  writeIORef ?uniqueSupply (r + 1)
  pure r

selectScope :: (IRnConstraints) => NameSpace -> Scope
selectScope = \case
  NameSpace'TermVar -> ?termVarScope
  NameSpace'TypeVar -> ?tyVarScope
  NameSpace'TypeConcrete -> ?tyConcreteScope

getVarId :: (IRnConstraints) => NameSpace -> NameFs -> Maybe Int
getVarId ns k = Map.lookup k (selectScope ns)

getExistingOrNewUnique :: NameSpace -> NameFs -> RnM Int
getExistingOrNewUnique ns name = maybe newUnique pure (getVarId ns name)

getExistingUnique :: NameSpace -> BNFC'Position -> NameFs -> RnM Int
getExistingUnique ns pos name =
  case getVarId ns name of
    Nothing ->
      dieRn
        RnError'UnboundTypeVariable
          { name
          , srcSpan = convertPositionToSrcSpan pos
          }
    Just u -> pure u

runWithScope :: NameSpace -> Scope -> RnM a -> RnM a
runWithScope ns scope act =
  case ns of
    NameSpace'TermVar -> let ?termVarScope = scope in act
    NameSpace'TypeVar -> let ?tyVarScope = scope in act
    NameSpace'TypeConcrete -> let ?tyConcreteScope = scope in act

withNameInScope :: NameSpace -> Name -> RnM a -> RnM a
withNameInScope ns name act = do
  let scope = Map.insert name.nameOcc.occNameFS name.nameUnique (selectScope ns)
  runWithScope ns scope act

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
          $ (selectScope ns)
  runWithScope ns scope act

convertProgram :: Abs.Program -> RnM (BT.SynTerm BT.CompRn)
convertProgram (Abs.Program _ program) = convertAbsToBT program

parseInputText :: T.Text -> RnM (BT.SynTerm BT.CompRn)
parseInputText input = do
  let
    parsed = parseWith pProgram input
  case parsed of
    -- TODO parse error message
    Left err -> do
      let lexerError = getLineAndColumnFromError err
      dieRn $ case lexerError of
        Just (lineNumber, columnNumber) ->
          RnError'LexerError
            { lineNumber = lineNumber - 1
            , columnNumber = columnNumber - 1
            , currentFilePath = ?currentFilePath
            }
        Nothing -> RnError'Unknown{message = err}
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

-- TODO use megaparsec
getLineAndColumnFromError :: String -> Maybe (Int, Int)
getLineAndColumnFromError s =
  s
    & filter (/= ',')
    & words
    & filter (isDigit . head)
    & \x -> case x of
      [lineNumber, columnNumber] ->
        pure (read lineNumber, read columnNumber)
      _ -> Nothing

class ConvertAbsToBT a where
  type To a
  convertAbsToBT :: (IRnConstraints) => a -> (To a)

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
    Abs.ExpBool pos val -> do
      let val' = case val of
            Abs.BoolTrue _ -> True
            Abs.BoolFalse _ -> False
      pure $ SynTerm'Lit (convertPositionToSrcSpan pos) (SynLit'Bool val')
    Abs.ExpCon _ (Abs.Con pos (Abs.NameUpperCase name)) ->
      pure $ SynTerm'Lit (convertPositionToSrcSpan pos) (SynLit'Con name)
    Abs.ExpApp pos term1 term2 -> do
      term1' <- convertAbsToBT term1
      term2' <- convertAbsToBT term2
      pure $ SynTerm'App (convertPositionToSrcSpan pos) term1' term2'
    Abs.ExpAbs pos var term -> do
      var' <- convertAbsToBT var True
      term' <- withNameInScope NameSpace'TermVar var' (convertAbsToBT term)
      pure $ SynTerm'Lam (convertPositionToSrcSpan pos) var' term'
    Abs.ExpAbsAnno pos var ty term -> do
      var' <- convertAbsToBT var True
      ty' <- convertAbsToBT ty
      term' <- withNameInScope NameSpace'TermVar var' (convertAbsToBT term)
      pure $ SynTerm'ALam (convertPositionToSrcSpan pos) var' ty' term'
    Abs.ExpLet pos var term1 term2 -> do
      var' <- convertAbsToBT var True
      -- TODO should the let-expression be recursive
      -- and the var be brought into scope of the assigned term?
      term1' <- convertAbsToBT term1
      term2' <- withNameInScope NameSpace'TermVar var' (convertAbsToBT term2)
      pure $ SynTerm'Let (convertPositionToSrcSpan pos) var' term1' term2'
    Abs.ExpAnno pos term ty -> do
      term' <- convertAbsToBT term
      ty' <- convertAbsToBT ty
      pure $ SynTerm'Ann (convertPositionToSrcSpan pos) term' ty'

instance ConvertAbsToBT Abs.Var where
  type To Abs.Var = Bool -> IO Name
  convertAbsToBT (Abs.Var pos (Abs.NameLowerCase name)) needUnique = do
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

instance ConvertAbsToBT Abs.TypeVariable where
  type To Abs.TypeVariable = IO Name
  convertAbsToBT (Abs.TypeVariableName pos (Abs.NameLowerCase name)) = do
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

instance ConvertAbsToBT Abs.Type where
  type To Abs.Type = IO (SynType CompRn)
  convertAbsToBT = \case
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
      tys' <- forM tys convertAbsToBT
      ty' <- withNamesInScope NameSpace'TypeVar tys' (convertAbsToBT ty)
      pure $ SynType'ForAll (convertPositionToSrcSpan pos) tys' ty'
    Abs.TypeFunc pos ty1 ty2 ->
      SynType'Fun (convertPositionToSrcSpan pos)
        <$> (convertAbsToBT ty1)
        <*> (convertAbsToBT ty2)

-- ==============================================
-- Renamer errors
-- ==============================================

-- | A renamer exception.
data RnError
  = -- TODO make a parser error
    RnError'LexerError {currentFilePath :: FastString, lineNumber :: Int, columnNumber :: Int}
  | RnError'Unknown {message :: String}
  | RnError'ForallBindsNoTvs {srcSpan :: SrcSpan}
  | RnError'UnboundTypeVariable {name :: NameFs, srcSpan :: SrcSpan}

-- | A renamer exception that can capture the 'callStack'
-- at the 'throw' side.
--
-- https://maksbotan.github.io/posts/2021-01-20-callstacks.html
data RnErrorWithCallStack where
  RnErrorWithCallStack :: (HasCallStack) => RnError -> RnErrorWithCallStack

-- | Fail unconditionally with a renamer exception.
dieRn :: (HasCallStack) => RnError -> IO a
dieRn rnError = throw (RnErrorWithCallStack rnError)

instance Pretty' RnError where
  pretty' = \case
    RnError'LexerError{currentFilePath, lineNumber, columnNumber} ->
      vsep'
        [ "Lexer error at:"
        , indent 2 $
            (pretty' currentFilePath <> ":")
              <> (pretty' (lineNumber + 1) <> ":")
              <> (pretty' (columnNumber + 1))
        ]
    RnError'Unknown{message} ->
      vsep'
        [ "Unknown error:"
        , prettyIndent message
        ]
    RnError'ForallBindsNoTvs{srcSpan} ->
      vsep'
        [ "`forall' binds no type variables at:"
        , prettyIndent srcSpan
        ]
    RnError'UnboundTypeVariable{srcSpan, name} ->
      vsep'
        [ "Unbound type variable:"
        , prettyIndent name
        , "at:"
        , prettyIndent srcSpan
        ]

instance Exception RnError

instance Pretty' RnErrorWithCallStack where
  pretty' (RnErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]

instance Exception RnErrorWithCallStack
