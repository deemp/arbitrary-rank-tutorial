module Language.Arralac.Evaluator.Whnf where

import Control.Monad.Foil (Distinct, Scope, identitySubst)
import Control.Monad.Free.Foil (substitute)
import Language.Arralac.Core.CoreNameBinder
import Language.Arralac.Core.Scoped

whnf :: (Distinct n) => Scope n -> SCore n -> SCore n
whnf scope = \case
  SCore'App fun arg ->
    case whnf scope fun of
      SCore'Lam binder body ->
        let subst = addSubst identitySubst binder arg
         in whnf scope (substitute scope subst body)
      fun' -> SCore'App fun' arg
  SCore'Let binder body rhs ->
    let subst = addSubst identitySubst binder body
     in whnf scope (substitute scope subst rhs)
  t -> t
