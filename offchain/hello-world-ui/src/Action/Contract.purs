module HelloWorld.Action.Contract where

import HelloWorld.ContractID (ContractID)

data Action
  = Activate
  | Initialize ContractID
  | Increment ContractID
  | Read ContractID
