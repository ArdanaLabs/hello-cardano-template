{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Coinbase.API (CoinbaseResponse(..), CoinbaseError(..), SpotPriceUnsafe(..), pricesAPIProxy) where

import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Servant.API

-- lastLowerCharFieldLabelModifier :: Options
-- lastLowerCharFieldLabelModifier = defaultOptions { fieldLabelModifier = (: []) . toLower . fromJust . (findLast isUpper) }

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}

pricesAPIProxy :: Proxy PricesAPI
pricesAPIProxy = Proxy

type PricesAPI = "prices" :> SpotPriceAPI

-- https://developers.coinbase.com/api/v2#get-spot-price
type SpotPriceAPI = Capture "currency_pair" T.Text :> "spot" :> QueryParam "date" T.Text :>  Get '[JSON] (CoinbaseResponse SpotPriceUnsafe)

data CoinbaseError = CoinbaseError {
  _id :: T.Text
, _message :: T.Text
} deriving (Eq, Generic, Ord, Show)
instance FromJSON CoinbaseError where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON CoinbaseError where
  toJSON = genericToJSON dropLeadingUnderscoreOptions

data CoinbaseResponse a = CoinbaseResponse {
  _errors :: Maybe [ CoinbaseError ]
, _data :: Maybe a
} deriving (Eq, Generic, Ord, Show)

instance FromJSON a => FromJSON (CoinbaseResponse a) where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON a => ToJSON (CoinbaseResponse a) where
  toJSON = genericToJSON dropLeadingUnderscoreOptions

data SpotPriceUnsafe = SpotPriceUnsafe {
  _amount :: T.Text
, _base :: T.Text
, _currency :: T.Text
} deriving (Eq, Generic, Ord, Show)

instance FromJSON SpotPriceUnsafe where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON SpotPriceUnsafe where
  toJSON = genericToJSON dropLeadingUnderscoreOptions
