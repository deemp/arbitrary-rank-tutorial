module Language.Arralac.Interpreter.Whnf where

import Control.Monad.Foil (Distinct, Scope, identitySubst)
import Control.Monad.Free.Foil (substitute)
import Language.Arralac.Core.AST
import Language.Arralac.Core.CoreNameBinder

whnf :: (Distinct n) => Scope n -> CoreE n -> CoreE n
whnf scope = \case
  AppE fun arg ->
    case whnf scope fun of
      LamE binder body ->
        let subst = addSubst identitySubst binder arg
         in whnf scope (substitute scope subst body)
      fun' -> AppE fun' arg
  LetE binder body rhs ->
    let subst = addSubst identitySubst binder body
     in whnf scope (substitute scope subst rhs)
  t -> t
