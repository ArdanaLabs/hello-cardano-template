{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Kraken.API (KrakenResponse(..), AssetTickerInfoUnsafe(..), AssetTickerInfo(..), marketDataAPIProxy, eitherDouble, findLast) where

import Data.Aeson
import Data.Proxy
import Data.Char (isUpper, toLower)
import qualified Data.Text as T
import Data.Text.Read (double)
import Data.Monoid
import Data.Maybe
import qualified Data.Map as Map
import GHC.Generics
import Servant.API

findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
findLast p = getLast . foldMap (\x -> if p x
                                        then Last (Just x)
                                        else Last Nothing)

lastLowerCharFieldLabelModifier :: Options
lastLowerCharFieldLabelModifier = defaultOptions { fieldLabelModifier = (: []) . toLower . fromJust . (findLast isUpper) }

dropLeadingUnderscoreOptions :: Options
dropLeadingUnderscoreOptions = defaultOptions {fieldLabelModifier = drop 1}


type MarketDataAPI = "public" :> TickerInformationAPI

marketDataAPIProxy :: Proxy MarketDataAPI
marketDataAPIProxy = Proxy

newtype Price = Price Double deriving (Eq, Fractional, Generic, Num, Ord, Read, Show, FromJSON, ToJSON)
newtype LotVolume = LotVolume Double deriving (Eq, Fractional, Generic, Num, Ord, Read, Show, FromJSON, ToJSON)

data AssetTickerInfo = AssetTickerInfo {
  _ask :: (Price, Integer, LotVolume)
, _bid :: (Price, Integer, LotVolume)
, _lastTradeClosed :: (Price, LotVolume)
, _volume :: (Double, Double)
, _volumeWeightedAveragePrice :: (Price, Price)
, _numberOfTrades :: (Integer, Integer)
, _low :: (Price, Price)
, _high :: (Price, Price)
, _todaysOpeningPrice :: Price
} deriving (Eq, Generic, Ord, Show)

eitherDouble :: T.Text -> Either String Double
eitherDouble text = do
  res <- double text
  case res of
    (x, "") -> Right x
    (_, remainder) -> Left $ "Unable to read " <> (T.unpack remainder) <> " from input " <> (T.unpack text)

-- toAssetTickerInfo :: AssetTickerInfoUnsafe -> Either String AssetTickerInfo
-- toAssetTickerInfo assetTickerInfoUnsafe = do
--   askPrice <- eitherDouble $ _unsafeAsk assetTickerInfoUnsafe
--   askWholeLotVolume <- eitherDouble $ _unsafeAsk assetTickerInfoUnsafe

data KrakenResponse a = KrakenResponse {
  _error :: [ T.Text ]
, _result :: Map.Map T.Text a
} deriving (Eq, Generic, Ord, Show)
instance FromJSON a => FromJSON (KrakenResponse a) where
  parseJSON = genericParseJSON dropLeadingUnderscoreOptions
instance ToJSON a => ToJSON (KrakenResponse a) where
  toJSON = genericToJSON dropLeadingUnderscoreOptions

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

-- https://docs.kraken.com/rest/#operation/getTickerInformation
type TickerInformationAPI = "Ticker" :> QueryParam "pair" T.Text :> Get '[JSON] (KrakenResponse AssetTickerInfoUnsafe)
