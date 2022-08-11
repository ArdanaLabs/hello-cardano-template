{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.Binance.API (PriceResponse (..), tickerAPIProxy) where

import Data.Aeson
import Data.Proxy
import Data.Text qualified as T
import GHC.Generics
import Servant.API

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}

tickerAPIProxy :: Proxy TickerAPI
tickerAPIProxy = Proxy

-- https://binance-docs.github.io/apidocs/spot/en/#symbol-price-ticker
type TickerAPI = "ticker" :> "price" :> QueryParam "symbol" T.Text :> Get '[JSON] PriceResponse

data PriceResponse = PriceResponse
  { _symbol :: T.Text
  , _price :: T.Text
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON PriceResponse where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON PriceResponse where
  toJSON = genericToJSON dropLeadingUnderscoreOptions
