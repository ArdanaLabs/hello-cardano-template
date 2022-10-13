module HelloWorld.Store where

import Prelude

import Contract.Monad (ConfigParams)
import Contract.Transaction (TransactionInput)
import Ctl.Internal.Serialization.Address (NetworkId)
import Data.Maybe (Maybe(..))

data Wallet
  = KeyWallet
  | NamiWallet NetworkId

derive instance eqWallet :: Eq Wallet

type Store =
  { wallet :: Wallet
  , contractConfig :: ConfigParams ()
  , lastOutput :: Maybe TransactionInput
  }

data Action
  = SetLastOutput TransactionInput
  | ResetLastOutput

reduce :: Store -> Action -> Store
reduce store = case _ of
  SetLastOutput lastOutput ->
    store { lastOutput = Just lastOutput }
  ResetLastOutput ->
    store { lastOutput = Nothing }
