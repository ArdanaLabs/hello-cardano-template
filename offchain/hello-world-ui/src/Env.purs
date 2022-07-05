module HelloWorld.Env where

import Prelude

newtype BaseURL = BaseURL String

derive instance eqBaseURL :: Eq BaseURL
derive instance ordBaseURL :: Ord BaseURL

newtype Wallet = Wallet String

derive instance eqWallet :: Eq Wallet
derive instance ordWallet :: Ord Wallet
instance Show Wallet where
  show (Wallet wallet) = wallet

type Env =
  { baseURL :: BaseURL
  , wallet :: Wallet
  }
