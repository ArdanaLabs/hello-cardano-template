module HelloWorld.Store where

import Contract.Monad (ConfigParams)
import Contract.Transaction (TransactionInput)
import Data.Maybe (Maybe(..))

type Store =
  { contractConfig :: ConfigParams ()
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
