{-# LANGUAGE DeriveGeneric #-}
module PriceData (PriceData(..), Price(..), binancePriceData, coinbasePriceData, huobiPriceData, krakenPriceData, kucoinPriceData) where

import Data.Aeson
import Data.GenValidity
import GHC.Generics

newtype Price = Price Double deriving (Eq, Generic, Ord, Show)

instance Validity Price where
  validate (Price p) =
    mconcat
      [ annotate p "Price",
        check (p >= 0.0) "The price is positive.",
        check (p <= 1000000.0) "The price is within a reasonable range"
      ]

instance GenValid Price

binancePriceData :: Double -> PriceData
binancePriceData price = PriceData {
                           _binancePrice = Just price
                         , _coinbasePrice = Nothing
                         , _huobiPrice = Nothing
                         , _krakenPrice = Nothing
                         , _kucoinPrice = Nothing
                         }

coinbasePriceData :: Double -> PriceData
coinbasePriceData price = PriceData {
                            _binancePrice = Nothing
                          , _coinbasePrice = Just price
                          , _huobiPrice = Nothing
                          , _krakenPrice = Nothing
                          , _kucoinPrice = Nothing
                          }

huobiPriceData :: Double -> Double -> PriceData
huobiPriceData ask bid = PriceData {
                            _binancePrice = Nothing
                          , _coinbasePrice = Nothing
                          , _huobiPrice = Just (ask, bid)
                          , _krakenPrice = Nothing
                          , _kucoinPrice = Nothing
                          }

krakenPriceData :: Double -> PriceData
krakenPriceData price = PriceData {
                          _binancePrice = Nothing
                        , _coinbasePrice = Nothing
                        , _huobiPrice = Nothing
                        , _krakenPrice = Just price
                        , _kucoinPrice = Nothing
                        }

kucoinPriceData :: Double -> PriceData
kucoinPriceData price = PriceData {
                          _binancePrice = Nothing
                        , _coinbasePrice = Nothing
                        , _huobiPrice = Nothing
                        , _krakenPrice = Nothing
                        , _kucoinPrice = Just price
                        }

data PriceData = PriceData {
  _binancePrice :: Maybe Double
, _coinbasePrice :: Maybe Double
, _huobiPrice :: Maybe (Double, Double) -- ask bid
, _krakenPrice :: Maybe Double
, _kucoinPrice :: Maybe Double
} deriving (Eq, Generic, Ord, Show)

instance Validity PriceData
instance GenValid PriceData
instance ToJSON PriceData
instance FromJSON PriceData
