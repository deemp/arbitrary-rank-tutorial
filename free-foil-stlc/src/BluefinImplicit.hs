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
import Control.Monad (forever, when)
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

-- State

-- evalState :: forall s (es :: Effects) a. s -> (forall (e :: Effects). State s e -> Eff (e :& es) a) -> Eff es a
type EvalStateI h = forall s es a. s -> (forall e. (e :> (e :& es)) => (Eval (StateExp h s e)) => Eff (e :& es) a) -> Eff es a

-- get :: forall (e :: Effects) (es :: Effects) s. (e :> es) => State s e -> Eff es s
type GetI h = forall e s es. (e :> es) => (Eval (StateExp h s e)) => Eff es s

-- modify :: forall (e :: Effects) (es :: Effects) s. (e :> es) => State s e -> (s -> s) -> Eff es ()
type ModifyI h = forall e es s. (e :> es) => (Eval (StateExp h s e)) => (s -> s) -> Eff es ()

-- Reader

-- runReader :: forall r (es :: Effects) a. r -> (forall (e :: Effects). Reader r e -> Eff (e :& es) a) -> Eff es a
type RunReaderI h = forall r es a. r -> (forall e. (e :> (e :& es)) => (Eval (ReaderExp h r e)) => Eff (e :& es) a) -> Eff es a

-- ask :: forall (e :: Effects) (es :: Effects) r. (e :> es) => Reader r e -> Eff es r
type AskI h = forall e es r. (e :> es) => (Eval (ReaderExp h r e)) => Eff es r

-- IO

-- runEff :: forall a. (forall (e :: Effects) (es :: Effects). IOE e -> Eff (e :& es) a) -> IO a
type RunEffI h = forall r. (forall es. (Eval (IOExp h es)) => Eff es r) -> IO r

-- effIO :: forall (e :: Effects) (es :: Effects) a. (e :> es) => IOE e -> IO a -> Eff es a
type EffIOI h = forall e es r. (e :> es) => (Eval (IOExp h e)) => IO r -> Eff es r

-- User code

type instance Eval (StateExp "st" s es) = (?st :: State s es)
type instance Eval (StateExp "st1" s es) = (?st1 :: State s es)
type instance Eval (ReaderExp "r" s es) = (?r :: Reader s es)
type instance Eval (IOExp "io" es) = (?io :: IOE es)

evalStateI :: EvalStateI "st"
evalStateI s f = evalState s (\x -> let ?st = x in f)

evalStateI1 :: EvalStateI "st1"
evalStateI1 s f = evalState s (\x -> let ?st1 = x in f)

runReaderI :: RunReaderI "r"
runReaderI r f = runReader r (\x -> let ?r = x in f)

runEffI :: RunEffI "io"
runEffI f = runEff (\io -> let ?io = io in pushFirst f)

askI :: AskI "r"
askI = ask ?r

getI :: GetI "st"
getI = get ?st

getI1 :: GetI "st1"
getI1 = get ?st1

effIOI :: EffIOI "io"
effIOI = effIO ?io

modifyI :: ModifyI "st"
modifyI = modify ?st

modifyI1 :: ModifyI "st1"
modifyI1 = modify ?st1

countExampleI :: IO ()
countExampleI =
  runEffI do
    runReaderI @Int 2 do
      r <- askI
      evalStateI1 @Double 0 do
        evalStateI @Int r do
          withJump $ \break -> forever do
            int' <- getI
            when (int' >= 10) (jumpTo break)
            double' <- getI1
            effIOI (print ((fromIntegral int') + double'))
            modifyI (+ 1)
            modifyI1 (+ 0.1)