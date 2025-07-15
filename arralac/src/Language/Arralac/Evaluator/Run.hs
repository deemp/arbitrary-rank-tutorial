module Language.Arralac.Evaluator.Run where

import Control.Monad.Foil (S (..), emptyScope)
import Language.Arralac.Core.AST
import Language.Arralac.Core.ConvertZonked
import Language.Arralac.Evaluator.Whnf
import Language.Arralac.Pass.Types
import Language.Arralac.Syntax.TTG.SynTerm

data EvaluatorMode
  = EvaluatorMode'Whnf

runEvaluator :: EvaluatorMode -> SynTerm CompZn -> SCore VoidS
runEvaluator mode znTerm =
  case mode of
    EvaluatorMode'Whnf ->
      whnf emptyScope (convertZonked emptyScope znTerm)

-- TODO Delayed substitution and Normalization by Evaluation
