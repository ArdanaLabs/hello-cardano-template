{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.Kucoin.API (FiatPriceResponse (..), fiatPriceAPIProxy) where

import Data.Aeson
import Data.Map qualified as M
import Data.Proxy
import Data.Text qualified as T
import GHC.Generics
import Servant.API

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}

fiatPriceAPIProxy :: Proxy FiatPriceAPI
fiatPriceAPIProxy = Proxy

-- https://docs.kucoin.com/#get-fiat-price
type FiatPriceAPI = "v1" :> "prices" :> QueryParam "base" T.Text :> QueryParams "currencies" T.Text :> Get '[JSON] FiatPriceResponse

data FiatPriceResponse = FiatPriceResponse
  { _code :: T.Text
  , _data :: M.Map T.Text T.Text
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON FiatPriceResponse where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON FiatPriceResponse where
  toJSON = genericToJSON dropLeadingUnderscoreOptions
