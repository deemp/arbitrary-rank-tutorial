{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module BluefinImplicit where

import Bluefin.Internal
import Data.Kind (Constraint, Type)
import Prelude hiding (break)

-- Library code

-- from first-class-families
type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

-- Expressions for effects
data StateExp :: idx -> s -> es -> Exp Constraint
data ReaderExp :: idx -> r -> es -> Exp Constraint
data IOExp :: idx -> es -> Exp Constraint
data ExceptionExp :: idx -> e -> es -> Exp Constraint

-- State

-- evalState :: forall s (es :: Effects) a. s -> (forall (e :: Effects). State s e -> Eff (e :& es) a) -> Eff es a
type EvalStateI h = forall s (es :: Effects) a. s -> (forall (e :: Effects). (e :> (e :& es)) => (Eval (StateExp h s e)) => Eff (e :& es) a) -> Eff es a

-- get :: forall (e :: Effects) (es :: Effects) s. (e :> es) => State s e -> Eff es s
type GetI h = forall (e :: Effects) (es :: Effects) s. (e :> es) => (Eval (StateExp h s e)) => Eff es s

-- modify :: forall (e :: Effects) (es :: Effects) s. (e :> es) => State s e -> (s -> s) -> Eff es ()
type ModifyI h = forall (e :: Effects) (es :: Effects) s. (e :> es) => (Eval (StateExp h s e)) => (s -> s) -> Eff es ()

-- Reader

-- runReader :: forall r (es :: Effects) a. r -> (forall (e :: Effects). Reader r e -> Eff (e :& es) a) -> Eff es a
type RunReaderI h = forall r (es :: Effects) a. r -> (forall (e :: Effects). (e :> (e :& es)) => (Eval (ReaderExp h r e)) => Eff (e :& es) a) -> Eff es a

-- ask :: forall (e :: Effects) (es :: Effects) r. (e :> es) => Reader r e -> Eff es r
type AskI h = forall (e :: Effects) (es :: Effects) r. (e :> es) => (Eval (ReaderExp h r e)) => Eff es r

-- IO

-- runEff :: forall a. (forall (e :: Effects) (es :: Effects). IOE e -> Eff (e :& es) a) -> IO a
type RunEffI h = forall r. (forall (e :: Effects) (es :: Effects). (e :> (e :& es)) => (Eval (IOExp h e)) => Eff (e :& es) r) -> IO r

-- effIO :: forall (e :: Effects) (es :: Effects) a. (e :> es) => IOE e -> IO a -> Eff es a
type EffIOI h = forall (e :: Effects) (es :: Effects) a. (e :> es) => (Eval (IOExp h e)) => IO a -> Eff es a

-- Exception

-- try :: forall exn (es :: Effects) a. (forall (e :: Effects). Exception exn e -> Eff (e :& es) a) -> Eff es (Either exn a)
type TryI h = forall exn (es :: Effects) a. (forall (e :: Effects). (e :> (e :& es)) => (Eval (ExceptionExp h exn e)) => Eff (e :& es) a) -> Eff es (Either exn a)

-- User code

type instance Eval (StateExp "st" s es) = (?st :: State s es)
type instance Eval (StateExp "st1" s es) = (?st1 :: State s es)
type instance Eval (ReaderExp "r" s es) = (?r :: Reader s es)
type instance Eval (IOExp "io" es) = (?io :: IOE es)
type instance Eval (ExceptionExp "ex" s es) = (?ex :: Exception s es)

-- Reader

runReaderI :: RunReaderI "r"
runReaderI r f = runReader r (\x -> let ?r = x in f)

askI :: AskI "r"
askI = ask ?r

-- State

evalStateI :: EvalStateI "st"
evalStateI s f = evalState s (\x -> let ?st = x in f)

getI :: GetI "st"
getI = get ?st

-- IO

runEffI :: RunEffI "io"
runEffI f = runEff (\io -> let ?io = io in f)

effIOI :: EffIOI "io"
effIOI = effIO ?io

-- Exception

tryI :: TryI "ex"
tryI f = try (\x -> let ?ex = x in f)