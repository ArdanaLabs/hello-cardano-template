module HelloWorld.Env where

import Prelude

newtype BaseURL = BaseURL String

derive instance eqBaseURL :: Eq BaseURL
derive instance ordBaseURL :: Ord BaseURL

newtype ContractID = ContractID String

derive instance eqContractID :: Eq ContractID
derive instance ordContractID :: Ord ContractID

type Env =
  { baseURL :: BaseURL
  , contractID :: ContractID
  }
