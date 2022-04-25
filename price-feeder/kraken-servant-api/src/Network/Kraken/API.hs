{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Kraken.API (MarketDataAPI, KrakenResponse(..), AssetTickerInfoResponseUnsafe(..), AssetTickerInfoUnsafe(..), OHLCResponseUnsafe(..), marketDataAPIProxy, findLast) where

import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Char (isUpper, toLower)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import GHC.Generics
import Servant.API

import qualified Data.HashMap.Lazy as HML

findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
findLast p = getLast . foldMap (\x -> if p x
                                        then Last (Just x)
                                        else Last Nothing)

lastLowerCharFieldLabelModifier :: Options
lastLowerCharFieldLabelModifier = defaultOptions { fieldLabelModifier = (: []) . toLower . fromJust . (findLast isUpper) }

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}


type MarketDataAPI = "public" :> (TickerInformationAPI
                            :<|>  OHLCAPI
                      )

marketDataAPIProxy :: Proxy MarketDataAPI
marketDataAPIProxy = Proxy

-- https://docs.kraken.com/rest/#operation/getTickerInformation
type TickerInformationAPI = "Ticker" :> QueryParam "pair" T.Text :> Get '[JSON] (KrakenResponse AssetTickerInfoResponseUnsafe)

data KrakenResponse a = KrakenResponse {
  _error :: [ T.Text ]
, _result :: a
} deriving (Eq, Generic, Ord, Show)
instance FromJSON a => FromJSON (KrakenResponse a) where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON a => ToJSON (KrakenResponse a) where
  toJSON = genericToJSON dropLeadingUnderscoreOptions

newtype AssetTickerInfoResponseUnsafe = AssetTickerInfoResponseUnsafe (M.Map T.Text AssetTickerInfoUnsafe) deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

data AssetTickerInfoUnsafe = AssetTickerInfoUnsafe {
  _unsafeAsk :: (T.Text, T.Text, T.Text)
, _unsafeBid :: (T.Text, T.Text, T.Text)
, _unsafeLastTradeClosed :: (T.Text, T.Text)
, _unsafeVolume :: (T.Text, T.Text)
, _unsafeVolumeWeightedAveragePrice :: (T.Text, T.Text)
, _unsafeNumberOfTrades :: (Integer, Integer)
, _unsafeLow :: (T.Text, T.Text)
, _unsafeHigh :: (T.Text, T.Text)
, _unsafePriceTodaysOpening :: T.Text
} deriving (Eq, Generic, Ord, Show)

instance FromJSON AssetTickerInfoUnsafe where
  parseJSON = genericParseJSON lastLowerCharFieldLabelModifier
instance ToJSON AssetTickerInfoUnsafe where
  toJSON = genericToJSON lastLowerCharFieldLabelModifier

-- OHLC - open-high-low-chart: https://docs.kraken.com/rest/#operation/getOHLCData
type OHLCAPI = "OHLC" :> QueryParam "pair" T.Text :> QueryParam "interval" Integer :> QueryParam "since" POSIXTime :> Get '[JSON] (KrakenResponse OHLCResponseUnsafe)

data OHLCResponseUnsafe = OHLCResponseUnsafe {
  _ohlcResponseUnsafeLast :: POSIXTime
, _ohlcResponseUnsafePairs :: M.Map T.Text [(POSIXTime, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, Integer)]
} deriving (Eq, Generic, Ord, Show)

instance FromJSON OHLCResponseUnsafe where
    parseJSON = withObject "OHCLResponse" $ \objectMap -> do
      _last <- objectMap .: "last"
      let objectWithoutLast = Object $ HML.delete "last" objectMap
      maybe (parseFail "No pairs found") (return . (OHLCResponseUnsafe _last)) $ parseMaybe parseJSON objectWithoutLast

instance ToJSON OHLCResponseUnsafe where
  toJSON (OHLCResponseUnsafe lastResult pairsResult) = object [ "last" .= toJSON lastResult ] `unionObjects` toJSON pairsResult
    where unionObjects (Object x) (Object y) = Object $ x `HML.union` y
          unionObjects _ _ = undefined
