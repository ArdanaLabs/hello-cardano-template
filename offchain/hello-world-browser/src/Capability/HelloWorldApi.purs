module HelloWorld.Capability.HelloWorldApi where

import Prelude

import Data.Either (Either)
import Effect.Aff (Error)
import Halogen (HalogenM, lift)

newtype HelloWorldIncrement = HelloWorldIncrement Int

derive newtype instance eqHelloWorldIncrement :: Eq HelloWorldIncrement
derive newtype instance ordHelloWorldIncrement :: Ord HelloWorldIncrement

newtype FundsLocked = FundsLocked Number

instance showFundsLocked :: Show FundsLocked where
  show (FundsLocked fundsLocked) = show fundsLocked

class Monad m <= HelloWorldApi m where
  lock :: HelloWorldIncrement -> Int -> m (Either Error FundsLocked)
  increment :: HelloWorldIncrement -> m (Either Error Unit)
  redeem :: HelloWorldIncrement -> m (Either Error Unit)

instance helloWorldApiHalogenM :: HelloWorldApi m => HelloWorldApi (HalogenM st act slots msg m) where
  lock a = lift <<< lock a
  increment = lift <<< increment
  redeem = lift <<< redeem
