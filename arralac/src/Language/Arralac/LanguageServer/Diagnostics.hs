module Language.Arralac.LanguageServer.Diagnostics where

import Control.Exception (SomeException)
import Control.Exception.Base (Exception (..))
import Data.Foldable
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Language.Arralac.Parser.Error
import Language.Arralac.Renamer.Error
import Language.Arralac.Renamer.Types
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.RnVar
import Language.Arralac.Syntax.Local.TyVar.Tc
import Language.Arralac.Typechecker.Error
import Language.LSP.Protocol.Types (Diagnostic (..), DiagnosticSeverity (DiagnosticSeverity_Error), Position (..), Range (..))

srcSpanToRange :: SrcSpan -> Maybe Range
srcSpanToRange = \case
  UnhelpfulSpan{} -> Nothing
  RealSrcSpan r ->
    pure
      Range
        { _start =
            Position
              { _line = fromIntegral r.srcSpanSLine
              , _character = fromIntegral r.srcSpanSCol
              }
        , _end =
            Position
              { _line = fromIntegral r.srcSpanELine
              , _character = fromIntegral r.srcSpanECol
              }
        }

mkDiagnostic :: (Show a) => Range -> a -> Diagnostic
mkDiagnostic range err =
  Diagnostic
    { _range = range
    , _severity = Just DiagnosticSeverity_Error
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = Nothing
    , -- TODO provide source
      _message = T.pack $ show err
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }

mkRnErrorDiagnostics :: SomeException -> Maybe [Diagnostic]
mkRnErrorDiagnostics e = do
  RnErrorWithCallStack err <- fromException e
  case err of
    RnError'ForallBindsNoTvs{} -> do
      range <- srcSpanToRange err.srcSpan
      pure [mkDiagnostic range err]
    RnError'UnboundTypeVariable{} -> do
      range <- srcSpanToRange err.srcSpan
      pure [mkDiagnostic range err]
    RnError'UnboundTermVariable{} -> do
      range <- srcSpanToRange err.srcSpan
      pure [mkDiagnostic range err]
    RnError'LetOccursCheckFailed{} -> do
      rangeLetLhs <- srcSpanToRange err.letOccursCheckInfo.letSrcSpan
      rangeLetLhsOcc <- srcSpanToRange err.letLhsOcc.varName.nameLoc
      pure [mkDiagnostic rangeLetLhs err, mkDiagnostic rangeLetLhsOcc err]

mkParserErrorDiagnostics :: SomeException -> Maybe [Diagnostic]
mkParserErrorDiagnostics e = do
  ParserErrorWithCallStack err <- fromException e
  case err of
    ParserError'LexerError{} -> do
      let range =
            Range
              { _start =
                  Position
                    { _line = fromIntegral err.lineNumber
                    , _character = fromIntegral err.columnNumber
                    }
              , _end =
                  Position
                    { _line = fromIntegral err.lineNumber
                    , _character = fromIntegral err.columnNumber
                    }
              }
      pure [mkDiagnostic range err]
    ParserError'ParserError{} -> do
      let range =
            Range
              { _start =
                  Position
                    { _line = fromIntegral err.lineNumber
                    , _character = fromIntegral err.columnNumber
                    }
              , _end =
                  Position
                    { _line = fromIntegral err.lineNumber + 1
                    , _character = fromIntegral err.columnNumber + 1
                    }
              }
      pure [mkDiagnostic range err]
    ParserError'Unknown{} -> pure []

mkTcErrorDiagnostics :: SomeException -> Maybe [Diagnostic]
mkTcErrorDiagnostics e = do
  TcErrorWithCallStack err <- fromException e
  case err of
    TcError'UndefinedVariable{} -> do
      range <- srcSpanToRange err.rnVar.varName.nameLoc
      pure [mkDiagnostic range err]
    TcError'UnboundVariable{} -> do
      range <- srcSpanToRange err.var.varName.nameLoc
      pure [mkDiagnostic range err]
    --  TODO
    -- TcError'UnexpectedType{} -> do
    --   vsep' $
    --     [ "Expected the type:"
    --     , prettyIndent err.expected
    --     , "but got the type:"
    --     , prettyIndent err.actual
    --     ]
    --       <> prettyThingLocation err.thing
    -- TcError'UnifyingBoundTypes{} ->
    --   vsep' $
    --     [ "Trying to unify type:"
    --     , prettyIndent err.ty1
    --     , "with type:"
    --     , prettyIndent err.ty2
    --     ]
    --       <> prettyThingLocation err.thing
    -- TcError'ExpectedFlexiVariables{} ->
    --   vsep'
    --     [ "Expected all variables to be Flexi, but these are not:"
    --     , prettyIndent err.tvs
    --     ]
    -- TcError'UnknownConcreteType{} ->
    --   vsep'
    --     [ "Unknown concrete type:"
    --     , prettyIndent err.name
    --     ]
    -- TcError'ExpectedAllMetavariables{} ->
    --   vsep'
    --     [ "Expected all variables to be metavariables, but got:"
    --     , prettyIndent err.tvs
    --     ]
    -- TcError'CannotUnify{} ->
    --   vsep' $
    --     [ "Cannot unify type:"
    --     , prettyIndent err.ty1
    --     , "with type:"
    --     , prettyIndent err.ty2
    --     ]
    --       <> prettyThingLocation err.thing
    _ -> Nothing

mkErrorDiagnostics :: SomeException -> [Diagnostic]
mkErrorDiagnostics e =
  fold $
    catMaybes
      [ mkParserErrorDiagnostics e
      , mkRnErrorDiagnostics e
      , mkTcErrorDiagnostics e
      ]