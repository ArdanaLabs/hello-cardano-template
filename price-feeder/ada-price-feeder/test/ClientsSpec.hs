{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module ClientsSpec (spec) where

-- import Data.Either (isLeft)
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

spec :: Spec
spec = do
  testGetBinancePrice
  testGetCoinbasePrice
  testGetHuobiPrice
  testGetKrakenPrice
  testGetKucoinPrice

testGetBinancePrice :: Spec
testGetBinancePrice = let expectedPrice = 0.79260000 in
  servantSpec tickerAPIProxy (mockTickerPriceHandler expectedPrice) $ do
    describe "getBinancePrice" $ do
      it "should successfully get the price" $ \clientEnv -> do
        getBinancePrice clientEnv >>= (`shouldBe` expectedPrice)

testGetCoinbasePrice :: Spec
testGetCoinbasePrice = let expectedPrice = 0.79260000 in
  servantSpec pricesAPIProxy (mockSpotPriceHandler expectedPrice) $ do
    describe "getCoinbasePrice" $ do
      it "should successfully get the price" $ \clientEnv -> do
        getCoinbasePrice clientEnv >>= (`shouldBe` expectedPrice)
testGetHuobiPrice :: Spec
testGetHuobiPrice = let expectedAsk = 0.79260000; expectedBid = 0.79260000 in
  servantSpec marketDataAPIProxy (mockGetTick expectedAsk expectedBid) $ do
    describe "getHuobiPrice" $ do
      it "should successfully get the price" $ \clientEnv -> do
        getHuobiPrice clientEnv >>= (`shouldBe` ((expectedAsk + expectedBid) / 2))

testGetKrakenPrice :: Spec
testGetKrakenPrice = let expectedPrice = 0.79260000 in
  servantSpec Kraken.marketDataAPIProxy (marketDataServer expectedPrice) $ do
    describe "getKrakenPrice" $ do
      it "should successfully get the price" $ \clientEnv -> do
        getKrakenPrice clientEnv >>= (`shouldBe` expectedPrice)

testGetKucoinPrice :: Spec
testGetKucoinPrice = let expectedPrice = 0.79260000 in
  servantSpec fiatPriceAPIProxy (mockFiatPriceHandler expectedPrice) $ do
    describe "getKucoinPrice" $ do
      it "should successfully get the price" $ \clientEnv -> do
        getKucoinPrice clientEnv >>= (`shouldBe` expectedPrice) 
