module HelloWorld.Store where

import Data.Maybe (Maybe(..))
import HelloWorld.Types (ContractConfig)

type Store =
  { contractConfig :: Maybe ContractConfig
  }

data Action = SetContractConfig ContractConfig

reduce :: Store -> Action -> Store
reduce store = case _ of
  SetContractConfig contractConfig ->
    store { contractConfig = Just contractConfig }
