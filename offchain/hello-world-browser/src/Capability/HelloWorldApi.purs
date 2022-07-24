module HelloWorld.Capability.HelloWorldApi where

import Prelude

import Halogen (HalogenM, lift)

newtype ScriptAddress = ScriptAddress Int

derive newtype instance eqScriptAddress :: Eq ScriptAddress
derive newtype instance ordScriptAddress :: Ord ScriptAddress

newtype FundsLocked = FundsLocked Number

instance showFundsLocked :: Show FundsLocked where
  show (FundsLocked fundsLocked) = show fundsLocked

class Monad m <= HelloWorldApi m where
  lock :: ScriptAddress -> Int -> m FundsLocked
  increment :: ScriptAddress -> Int -> m Unit
  redeem :: ScriptAddress -> m Unit

instance helloWorldApiHalogenM :: HelloWorldApi m => HelloWorldApi (HalogenM st act slots msg m) where
  lock a = lift <<< lock a
  increment a = lift <<< increment a
  redeem = lift <<< redeem
