module HelloWorld.Capability.HelloWorldApi where

import Contract.Prelude

import Contract.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.Either (Either)
import Halogen (HalogenM, lift)
import HelloWorld.Error (HelloWorldBrowserError)

newtype HelloWorldIncrement = HelloWorldIncrement Int

derive newtype instance eqHelloWorldIncrement :: Eq HelloWorldIncrement
derive newtype instance ordHelloWorldIncrement :: Ord HelloWorldIncrement

newtype FundsLocked = FundsLocked Number

instance showFundsLocked :: Show FundsLocked where
  show (FundsLocked fundsLocked) = show fundsLocked

class Monad m <= HelloWorldApi m where
  lock :: HelloWorldIncrement -> Int -> m (Either HelloWorldBrowserError (TransactionInput /\ FundsLocked))
  increment :: HelloWorldIncrement -> TransactionInput -> m (Either HelloWorldBrowserError TransactionInput)
  redeem :: HelloWorldIncrement -> TransactionInput -> m (Either HelloWorldBrowserError BigInt)
  unlock :: BigInt -> m (Either HelloWorldBrowserError Unit)
  getDatum :: TransactionInput -> m (Either HelloWorldBrowserError Int)
  resume :: HelloWorldIncrement -> m (Either HelloWorldBrowserError (TransactionInput /\ FundsLocked))

instance helloWorldApiHalogenM :: HelloWorldApi m => HelloWorldApi (HalogenM st act slots msg m) where
  lock a = lift <<< lock a
  increment a = lift <<< increment a
  redeem a = lift <<< redeem a
  unlock = lift <<< unlock
  getDatum = lift <<< getDatum
  resume = lift <<< resume
