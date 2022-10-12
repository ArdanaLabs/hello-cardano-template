module HelloWorld.Store where

import Contract.Monad (ConfigParams)
import Contract.Transaction (TransactionInput)
import Data.Maybe (Maybe(..))

data Wallet
  = KeyWallet (ConfigParams ())
  | NamiWallet

type Store =
  { wallet :: Wallet
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
