{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Network.Binance.ClientSpec (spec) where

import Data.Either (isLeft)
import Test.Syd
import Test.Syd.Servant

import Network.Binance.API (PriceUnsafe(..), tickerAPIProxy)
import Network.Binance.Client (getPrice)
import Network.Binance.Server.Mock (mockTickerPriceHandler)
import PriceData.Types (CurrencyPair(..))
import UnliftIO (tryAny)

spec :: Spec
spec = do
  testGetSpotPrice

testGetSpotPrice :: Spec
testGetSpotPrice = servantSpec tickerAPIProxy mockTickerPriceHandler $ do
  describe "getPrice" $ do
    it "should successfully return the price" $ \clientEnv -> do
      res <- liftIO $ getPrice clientEnv (CurrencyPair "ADA" "USDT")
      res `shouldBe` PriceUnsafe { _symbol = "ADAUSDT", _price = "0.79260000" }
    it "should throw an error" $ \clientEnv -> do
      eitherErr <- tryAny $ getPrice clientEnv (CurrencyPair "AD" "USDT")
      isLeft eitherErr `shouldBe` True
