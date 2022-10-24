module HelloWorld.Types where

import Prelude

import Contract.Monad (ConfigParams)

type ContractConfig = ConfigParams ()

data HelloWorldWallet
  = Nami
  | Eternl

derive instance eqHelloWorldWallet :: Eq HelloWorldWallet

instance showHelloWorldWallet :: Show HelloWorldWallet where
  show Nami = "Nami"
  show Eternl = "Eternl"
