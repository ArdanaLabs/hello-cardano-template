{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Network.Kraken.API (MarketDataAPI, KrakenResponse (..), AssetTickerInfoResponse (..), AssetTickerInfo (..), OHLCResponse (..), marketDataAPIProxy, findLast) where

import Data.Aeson
import Data.Aeson.KeyMap (delete, union)
import Data.Aeson.Types
import Data.Map qualified as M
import Data.Monoid
import Data.Proxy
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics
import Servant.API
import UnliftIO.Exception (impureThrow, stringException)

findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
findLast p =
  getLast
    . foldMap
      ( \x ->
          if p x
            then Last (Just x)
            else Last Nothing
      )

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}

type MarketDataAPI =
  "public"
    :> ( TickerInformationAPI
          :<|> OHLCAPI
       )

marketDataAPIProxy :: Proxy MarketDataAPI
marketDataAPIProxy = Proxy

-- https://docs.kraken.com/rest/#operation/getTickerInformation
type TickerInformationAPI = "Ticker" :> QueryParam "pair" T.Text :> Get '[JSON] (KrakenResponse AssetTickerInfoResponse)

data KrakenResponse a = KrakenResponse
  { _error :: [T.Text]
  , _result :: a
  }
  deriving (Eq, Generic, Ord, Show)
instance FromJSON a => FromJSON (KrakenResponse a) where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON a => ToJSON (KrakenResponse a) where
  toJSON = genericToJSON dropLeadingUnderscoreOptions

newtype AssetTickerInfoResponse = AssetTickerInfoResponse (M.Map T.Text AssetTickerInfo) deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

data AssetTickerInfo = AssetTickerInfo
  { _ask :: (T.Text, T.Text, T.Text)
  , _bid :: (T.Text, T.Text, T.Text)
  , _lastTradeClosed :: (T.Text, T.Text)
  , _volume :: (T.Text, T.Text)
  , _volumeWeightedAveragePrice :: (T.Text, T.Text)
  , _numberOfTrades :: (Integer, Integer)
  , _low :: (T.Text, T.Text)
  , _high :: (T.Text, T.Text)
  , _priceTodaysOpening :: T.Text
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON AssetTickerInfo where
  parseJSON = genericParseJSON lastLowerCharFieldLabelModifier
instance ToJSON AssetTickerInfo where
  toJSON = genericToJSON lastLowerCharFieldLabelModifier

lastLowerCharFieldLabelModifier :: Options
lastLowerCharFieldLabelModifier =
  defaultOptions
    { fieldLabelModifier =
        let f "_ask" = "a"
            f "_bid" = "b"
            f "_lastTradeClosed" = "c"
            f "_volume" = "v"
            f "_volumeWeightedAveragePrice" = "p"
            f "_numberOfTrades" = "t"
            f "_low" = "l"
            f "_high" = "h"
            f "_priceTodaysOpening" = "o"
            f other = other
         in f
    }

-- OHLC - open-high-low-chart: https://docs.kraken.com/rest/#operation/getOHLCData
type OHLCAPI = "OHLC" :> QueryParam "pair" T.Text :> QueryParam "interval" Integer :> QueryParam "since" POSIXTime :> Get '[JSON] (KrakenResponse OHLCResponse)

data OHLCResponse = OHLCResponse
  { _ohlcResponseLast :: POSIXTime
  , _ohlcResponsePairs :: M.Map T.Text [(POSIXTime, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, Integer)]
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON OHLCResponse where
  parseJSON = withObject "OHCLResponse" $ \objectMap -> do
    _last <- objectMap .: "last"
    let objectWithoutLast = Object $ delete "last" objectMap
    maybe (parseFail "No pairs found") (pure . OHLCResponse _last) $ parseMaybe parseJSON objectWithoutLast

instance ToJSON OHLCResponse where
  toJSON (OHLCResponse lastResult pairsResult) = object ["last" .= toJSON lastResult] `unionObjects` toJSON pairsResult
    where
      unionObjects (Object x) (Object y) = Object $ x `union` y
      unionObjects _ _ = impureThrow . stringException $ "Not able to union non-objects"
