module HelloWorld.ContractID where

import Prelude

newtype ContractID = ContractID String

derive instance eqContractID :: Eq ContractID
derive instance ordContractID :: Ord ContractID
