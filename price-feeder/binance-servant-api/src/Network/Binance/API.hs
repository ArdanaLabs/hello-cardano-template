{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Binance.API (PriceUnsafe(..), tickerAPIProxy) where

import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Servant.API

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}

tickerAPIProxy :: Proxy TickerAPI
tickerAPIProxy = Proxy

-- https://binance-docs.github.io/apidocs/spot/en/#symbol-price-ticker
type TickerAPI = "ticker" :> "price" :> QueryParam "symbol" T.Text :> Get '[JSON] PriceUnsafe

data PriceUnsafe = PriceUnsafe {
  _symbol :: T.Text
, _price :: T.Text
} deriving (Eq, Generic, Ord, Show)

instance FromJSON PriceUnsafe where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON PriceUnsafe where
  toJSON = genericToJSON dropLeadingUnderscoreOptions
