{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.Coinbase.API (CoinbaseResponse (..), CoinbaseError (..), SpotPrice (..), pricesAPIProxy) where

import Data.Aeson
import Data.Proxy
import Data.Text qualified as T
import GHC.Generics
import Servant.API

-- lastLowerCharFieldLabelModifier :: Options
-- lastLowerCharFieldLabelModifier = defaultOptions { fieldLabelModifier = (: []) . toLower . fromJust . (findLast isUpper) }

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}

pricesAPIProxy :: Proxy PricesAPI
pricesAPIProxy = Proxy

type PricesAPI = "prices" :> SpotPriceAPI

-- https://docs.cloud.coinbase.com/sign-in-with-coinbase/docs/api-prices#get-spot-price
type SpotPriceAPI = Capture "currency_pair" T.Text :> "spot" :> QueryParam "date" T.Text :> Get '[JSON] (CoinbaseResponse SpotPrice)

data CoinbaseError = CoinbaseError
  { _id :: T.Text
  , _message :: T.Text
  }
  deriving (Eq, Generic, Ord, Show)
instance FromJSON CoinbaseError where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON CoinbaseError where
  toJSON = genericToJSON dropLeadingUnderscoreOptions

data CoinbaseResponse a = CoinbaseResponse
  { _errors :: Maybe [CoinbaseError]
  , _data :: Maybe a
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON a => FromJSON (CoinbaseResponse a) where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON a => ToJSON (CoinbaseResponse a) where
  toJSON = genericToJSON dropLeadingUnderscoreOptions

data SpotPrice = SpotPrice
  { _amount :: T.Text
  , _base :: T.Text
  , _currency :: T.Text
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON SpotPrice where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON SpotPrice where
  toJSON = genericToJSON dropLeadingUnderscoreOptions
