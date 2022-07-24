module HelloWorld.Store where

import Contract.Monad (DefaultContractConfig)
import Data.Maybe (Maybe(..))
import Types.Transaction (TransactionInput)

type Store =
  { contractConfig :: DefaultContractConfig
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
