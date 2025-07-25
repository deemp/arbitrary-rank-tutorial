{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Renamer.Error where

import Control.Exception (Exception, throw)
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Language.Arralac.Prelude.Pretty
import Language.Arralac.Renamer.Types
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Type.Local.RnVar

-- | A renamer error.
data RnError
  = RnError'ForAllBindsNoTvs {srcSpan :: SrcSpan}
  | RnError'UnboundTermVariable {name :: NameFs, srcSpan :: SrcSpan}
  | RnError'UnboundTypeVariable {name :: NameFs, srcSpan :: SrcSpan}
  | RnError'LetOccursCheckFailed {letOccursCheckInfo :: LetOccursCheckInfo, letLhsOcc :: RnVar}

-- | A renamer error that can capture the 'callStack' at the 'throw' site.
--
-- https://maksbotan.github.io/posts/2021-01-20-callstacks.html
data RnErrorWithCallStack where
  RnErrorWithCallStack :: (HasCallStack) => RnError -> RnErrorWithCallStack

-- | Fail unconditionally with a 'RnErrorWithCallStack'.
dieRn :: (HasCallStack) => RnError -> IO a
dieRn err = throw (RnErrorWithCallStack err)

instance Pretty' RnError where
  pretty' err = case err of
    RnError'ForAllBindsNoTvs{} ->
      vsep'
        [ "`forall' binds no type variables at:"
        , prettyIndent err.srcSpan
        ]
    RnError'UnboundTypeVariable{} ->
      vsep'
        [ "Unbound type variable:"
        , prettyIndent err.name
        , "at:"
        , prettyIndent err.srcSpan
        ]
    RnError'UnboundTermVariable{} ->
      vsep'
        [ "Unbound term variable:"
        , prettyIndent err.name
        , "at:"
        , prettyIndent err.srcSpan
        ]
    RnError'LetOccursCheckFailed{} ->
      vsep'
        [ "Recursive `let'-binding at:"
        , prettyIndent err.letOccursCheckInfo.letSrcSpan
        , "Variable:"
        , prettyIndent err.letOccursCheckInfo.letLhs
        , "bound in a `let'-expression at:"
        , prettyIndent err.letOccursCheckInfo.letLhs.varName.nameLoc
        , "occurs in the RHS-expression at:"
        , prettyIndent err.letLhsOcc.varName.nameLoc
        ]

instance Exception RnError

instance Pretty' RnErrorWithCallStack where
  pretty' (RnErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]

instance Exception RnErrorWithCallStack
