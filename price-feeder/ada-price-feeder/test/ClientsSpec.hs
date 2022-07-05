{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ClientsSpec (spec) where

import Data.Aeson (encodeFile)
import Data.GenValidity
import Test.QuickCheck
import Test.Syd
import Test.Syd.Servant

import Clients
import Network.Binance.API (tickerAPIProxy)
import Network.Binance.Server.Mock (mockTickerPriceHandler)
import Network.Coinbase.API (pricesAPIProxy)
import Network.Coinbase.Server.Mock (mockSpotPriceHandler)
import Network.Huobi.API (marketDataAPIProxy)
import Network.Huobi.Server.Mock (mockGetTick)
import Network.Kraken.API qualified as Kraken (marketDataAPIProxy)
import Network.Kraken.Server.Mock (marketDataServer)
import Network.Kucoin.API (fiatPriceAPIProxy)
import Network.Kucoin.Server.Mock (mockFiatPriceHandler)

import PriceData (Price (..), binancePriceData, coinbasePriceData, huobiPriceData, krakenPriceData, kucoinPriceData)

approximately :: (Ord a, Num a) => a -> a -> a -> Bool
approximately theta x y =
  let _min = x - theta
      _max = x + theta
   in y >= _min && y <= _max

defaultTheta :: Double
defaultTheta = 0.0000001

spec :: Spec
spec = do
  testGetBinancePrice
  testGetCoinbasePrice
  testGetHuobiPrice
  testGetKrakenPrice
  testGetKucoinPrice

testGetBinancePrice :: Spec
testGetBinancePrice =
  let priceDataPath = "/tmp/binance.json"
   in servantSpec tickerAPIProxy (mockTickerPriceHandler priceDataPath) $ do
        describe "getBinancePrice" $ do
          it "should successfully get the price" $ \clientEnv -> do
            forAll (genValid @Price) $ \(Price expectedPrice) -> do
              liftIO $ encodeFile priceDataPath (binancePriceData expectedPrice)
              getBinancePrice clientEnv >>= (`shouldSatisfy` approximately defaultTheta expectedPrice)

testGetCoinbasePrice :: Spec
testGetCoinbasePrice =
  let priceDataPath = "/tmp/coinbase.json"
   in servantSpec pricesAPIProxy (mockSpotPriceHandler priceDataPath) $ do
        describe "getCoinbasePrice" $ do
          it "should successfully get the price" $ \clientEnv -> do
            forAll (genValid @Price) $ \(Price expectedPrice) -> do
              liftIO $ encodeFile priceDataPath (coinbasePriceData expectedPrice)
              getCoinbasePrice clientEnv >>= (`shouldSatisfy` approximately defaultTheta expectedPrice)

testGetHuobiPrice :: Spec
testGetHuobiPrice =
  let priceDataPath = "/tmp/huobi.json"
   in servantSpec marketDataAPIProxy (mockGetTick priceDataPath) $ do
        describe "getHuobiPrice" $ do
          it "should successfully get the price" $ \clientEnv -> do
            forAll (genValid @(Price, Price)) $ \(Price expectedAsk, Price expectedBid) -> do
              liftIO $ encodeFile priceDataPath (huobiPriceData expectedAsk expectedBid)
              getHuobiPrice clientEnv >>= (`shouldSatisfy` (approximately defaultTheta $ (expectedAsk + expectedBid) / 2))

testGetKrakenPrice :: Spec
testGetKrakenPrice =
  let priceDataPath = "/tmp/kraken.json"
   in servantSpec Kraken.marketDataAPIProxy (marketDataServer priceDataPath) $ do
        describe "getKrakenPrice" $ do
          it "should successfully get the price" $ \clientEnv -> do
            forAll (genValid @Price) $ \(Price expectedPrice) -> do
              liftIO $ encodeFile priceDataPath (krakenPriceData expectedPrice)
              getKrakenPrice clientEnv >>= (`shouldSatisfy` approximately defaultTheta expectedPrice)

testGetKucoinPrice :: Spec
testGetKucoinPrice =
  let priceDataPath = "/tmp/kucoin.json"
   in servantSpec fiatPriceAPIProxy (mockFiatPriceHandler priceDataPath) $ do
        describe "getKucoinPrice" $ do
          it "should successfully get the price" $ \clientEnv -> do
            forAll (genValid @Price) $ \(Price expectedPrice) -> do
              liftIO $ encodeFile priceDataPath (kucoinPriceData expectedPrice)
              getKucoinPrice clientEnv >>= (`shouldSatisfy` approximately defaultTheta expectedPrice)
