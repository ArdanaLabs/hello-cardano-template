{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Huobi.API (TickResponse(..), Tick(..), marketDataAPIProxy) where

import Data.Aeson
import Data.Proxy
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Text as T
import GHC.Generics
import Servant.API

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}

marketDataAPIProxy :: Proxy MarketDataAPI
marketDataAPIProxy = Proxy

-- https://binance-docs.github.io/apidocs/spot/en/#symbol-price-ticker
type MarketDataAPI = "market" :> "detail" :> "merged" :> QueryParam "symbol" T.Text :> Get '[JSON] TickResponse

data TickResponse = TickResponse {
  _channel :: T.Text
, _status :: T.Text
, _timestamp :: POSIXTime
, _tick :: Tick
} deriving (Eq, Generic, Ord, Show)

instance FromJSON TickResponse where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = let f "_channel" = "ch"
                                                                          f "_timestamp" = "ts"
                                                                          f other = (drop 1) other
                                                                      in f }
instance ToJSON TickResponse where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = let f "_channel" = "ch"
                                                                    f "_timestamp" = "ts"
                                                                    f other = (drop 1) other
                                                                in f }

data Tick = Tick {
  _id :: Integer
, _version :: Integer
, _open :: Double
, _close :: Double
, _low :: Double
, _high :: Double
, _amount :: Double
, _vol :: Double
, _count :: Integer
, _bid :: (Double, Double)
, _ask :: (Double, Double)
} deriving (Eq, Generic, Ord, Show)

instance FromJSON Tick where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON Tick where
  toJSON = genericToJSON dropLeadingUnderscoreOptions
