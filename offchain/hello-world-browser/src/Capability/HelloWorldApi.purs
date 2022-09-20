module HelloWorld.Capability.HelloWorldApi where

import Prelude

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
  lock :: HelloWorldIncrement -> Int -> m (Either HelloWorldBrowserError FundsLocked)
  increment :: HelloWorldIncrement -> m (Either HelloWorldBrowserError Unit)
  redeem :: HelloWorldIncrement -> m (Either HelloWorldBrowserError BigInt)
  unlock :: BigInt -> m (Either HelloWorldBrowserError Unit)

instance helloWorldApiHalogenM :: HelloWorldApi m => HelloWorldApi (HalogenM st act slots msg m) where
  lock a = lift <<< lock a
  increment = lift <<< increment
  redeem = lift <<< redeem
  unlock = lift <<< unlock
