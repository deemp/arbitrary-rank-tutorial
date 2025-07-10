{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Language.STLC.Interpreter.FreeFoil where

import Control.Monad.Foil (CoSinkable (..), DistinctEvidence (..), ExtEvidence (..))
import Control.Monad.Foil.Internal (DExt, Name (..), NameBinder (..), Scope (..), SinkableK, Substitution (..), unsafeDistinct, unsafeExt)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Generics.Kind.TH
import Language.STLC.Typing.Jones2007.BasicTypes (Pretty' (..))
import Language.STLC.Typing.Jones2007.BasicTypes qualified as BT
import Unsafe.Coerce (unsafeCoerce)

-- | A wrapper around 'NameBinder' that stores additional information.
data CoreNameBinder n l
  = CoreNameBinder
  { name :: BT.Name
  , nameBinder :: (NameBinder n l)
  }

deriveGenericK 'CoreNameBinder

-- ** Extending scopes

-- | \(O(\min(n,W))\).
-- Extend a scope with one name (safely).
-- Note that as long as the foil is used as intended,
-- the name binder is guaranteed to introduce a name
-- that does not appear in the initial scope.
extendScope :: CoreNameBinder n l -> Scope n -> Scope l
extendScope CoreNameBinder{nameBinder = UnsafeNameBinder (UnsafeName name)} (UnsafeScope scope) =
  UnsafeScope (IntSet.insert name scope)

-- ** Refreshing binders

-- TODO check scope

-- | Allocate a fresh binder for a given scope.
withFreshBinderUsingUnique ::
  Scope n ->
  BT.Name ->
  (forall l. CoreNameBinder n l -> r) ->
  r
withFreshBinderUsingUnique (UnsafeScope _scope) name cont =
  cont binder
 where
  binder =
    CoreNameBinder
      { name
      , nameBinder = UnsafeNameBinder (UnsafeName (name.nameUnique))
      }

-- | Unsafely declare that a given name (binder)
-- is already fresh in any scope @n'@.
unsafeAssertFresh ::
  forall n l n' l' r.
  CoreNameBinder n l ->
  ((DExt n' l') => CoreNameBinder n' l' -> r) ->
  r
unsafeAssertFresh binder cont =
  case unsafeDistinct @l' of
    Distinct -> case unsafeExt @n' @l' of
      Ext -> cont (unsafeCoerce binder)

-- | Safely produce a fresh name binder with respect to a given scope.
withFreshUsingUnique ::
  Scope n ->
  BT.Name ->
  (forall l. (DExt n l) => CoreNameBinder n l -> r) ->
  r
withFreshUsingUnique scope name cont = withFreshBinderUsingUnique scope name (`unsafeAssertFresh` cont)

-- * Safe substitions

-- | Extend substitution with a particular mapping.
addSubst ::
  Substitution e i o ->
  CoreNameBinder i i' ->
  e o ->
  Substitution e i' o
addSubst
  (UnsafeSubstitution env)
  (CoreNameBinder{nameBinder = UnsafeNameBinder (UnsafeName name')})
  ex =
    UnsafeSubstitution (IntMap.insert name' ex env)

instance CoSinkable CoreNameBinder where
  coSinkabilityProof _rename CoreNameBinder{name, nameBinder = UnsafeNameBinder name'} cont =
    cont unsafeCoerce CoreNameBinder{name, nameBinder = UnsafeNameBinder name'}

  withPattern f _ _ scope coreNameBinder coercion =
    (f scope coreNameBinder.nameBinder)
      (\x y -> coercion x CoreNameBinder{name = error "Impossible!", nameBinder = y})

instance SinkableK CoreNameBinder

newtype PrettyName n = PrettyName (Name n)

newtype PrettyNameBinder n l = PrettyNameBinder (NameBinder n l)

instance Pretty' (PrettyName n) where
  pretty' (PrettyName (UnsafeName n)) = "x_" <> pretty' n

instance Pretty' (PrettyNameBinder n l) where
  pretty' (PrettyNameBinder (UnsafeNameBinder name)) = pretty' (PrettyName name)

instance Pretty' (CoreNameBinder n l) where
  pretty' binder = pretty' (PrettyNameBinder binder.nameBinder)

