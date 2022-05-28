module HelloWorld.Endpoint where

import Prelude

import HelloWorld.ContractID (ContractID(..))
import HelloWorld.Env (BaseURL(..))

data Endpoint
  = Activate
  | Initialize ContractID
  | Increment ContractID
  | Read ContractID

derive instance eqEndpoint :: Eq Endpoint
derive instance ordEndpoint :: Ord Endpoint

toURL :: BaseURL -> Endpoint -> String
toURL (BaseURL baseURL) Activate = baseURL <> "/api/contract/activate"
toURL (BaseURL baseURL) (Initialize (ContractID contractID)) = baseURL <> "/api/contract/instance/" <> contractID <> "/endpoint/initialize"
toURL (BaseURL baseURL) (Increment (ContractID contractID)) = baseURL <> "/api/contract/instance/" <> contractID <> "/endpoint/increment"
toURL (BaseURL baseURL) (Read (ContractID contractID)) = baseURL <> "/api/contract/instance/" <> contractID <> "/endpoint/read"
