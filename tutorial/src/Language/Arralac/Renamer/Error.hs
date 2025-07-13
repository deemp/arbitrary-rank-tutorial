{-# OPTIONS_GHC -Wno-partial-fields #-}

module Language.Arralac.Renamer.Error where

import Control.Exception (Exception, throw)
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Language.Arralac.Renamer.Types
import Language.Arralac.Syntax.Local.Name
import Language.Arralac.Syntax.Local.RnVar
import Language.Arralac.Utils.Pretty

-- | A renamer exception.
data RnError
  = -- TODO make a parser error
    RnError'ForallBindsNoTvs {srcSpan :: SrcSpan}
  | RnError'UnboundTypeVariable {name :: NameFs, srcSpan :: SrcSpan}
  | RnError'LetOccursCheckFailed {letOccursCheckInfo :: LetOccursCheckInfo, letLhsOcc :: RnVar}

-- | A renamer exception that can capture the 'callStack' at the 'throw' site.
--
-- https://maksbotan.github.io/posts/2021-01-20-callstacks.html
data RnErrorWithCallStack where
  RnErrorWithCallStack :: (HasCallStack) => RnError -> RnErrorWithCallStack

-- | Fail unconditionally with a 'RnErrorWithCallStack'.
dieRn :: (HasCallStack) => RnError -> IO a
dieRn rnError = throw (RnErrorWithCallStack rnError)

instance Pretty' RnError where
  pretty' = \case
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
    RnError'LetOccursCheckFailed{letOccursCheckInfo, letLhsOcc} ->
      vsep'
        [ "Recursive `let'-binding at:"
        , prettyIndent letOccursCheckInfo.letSrcSpan
        , "Variable:"
        , prettyIndent letOccursCheckInfo.letLhs
        , "bound in a `let'-expression at:"
        , prettyIndent letOccursCheckInfo.letLhs.varName.nameLoc
        , "occurs in the RHS-expression at:"
        , prettyIndent letLhsOcc.varName.nameLoc
        ]

instance Exception RnError

instance Pretty' RnErrorWithCallStack where
  pretty' (RnErrorWithCallStack err) =
    vsep'
      [ pretty' (prettyCallStack callStack)
      , pretty' err
      ]

instance Exception RnErrorWithCallStack
