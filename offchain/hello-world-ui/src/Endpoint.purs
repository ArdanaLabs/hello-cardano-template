module HelloWorld.Endpoint where

import Prelude

data Endpoint
  = Initialize
  | Increment
  | Read

derive instance eqEndpoint :: Eq Endpoint
derive instance ordEndpoint :: Ord Endpoint
