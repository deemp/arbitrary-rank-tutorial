module Language.Arralac.Interpreter.Run where

import Control.Monad.Foil (S (..), emptyScope)
import Language.Arralac.Core.AST
import Language.Arralac.Core.ConvertZonked
import Language.Arralac.Interpreter.Whnf
import Language.Arralac.Prelude.Pass
import Language.Arralac.Syntax.TTG.SynTerm

data InterpreterMode
  = InterpreterMode'Whnf

runInterpreter :: InterpreterMode -> SynTerm CompZn -> CoreE VoidS
runInterpreter mode znTerm =
  case mode of
    InterpreterMode'Whnf ->
      whnf emptyScope (convertZonked emptyScope znTerm)