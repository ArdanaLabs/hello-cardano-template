{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Network.Coinbase.ClientSpec (spec) where

import Data.Time.Calendar (fromGregorianValid)

import Test.Syd
import Test.Syd.Servant

import Network.Coinbase.API (SpotPriceUnsafe(..), pricesAPIProxy)
import Network.Coinbase.Client (getSpotPrice)
import Network.Coinbase.Server.Mock (mockSpotPriceHandler)

spec :: Spec
spec = do
  testGetSpotPrice

testGetSpotPrice :: Spec
testGetSpotPrice = servantSpec pricesAPIProxy mockSpotPriceHandler $ do
  describe "getSpotPrice" $ do
    it "should successfully return the spot price" $ \clientEnv -> do
      res <- liftIO $ getSpotPrice clientEnv "BTC" "USD" Nothing
      res `shouldBe` SpotPriceUnsafe { _base = "BTC", _currency = "USD", _amount = "1232.44" }
    it "should successfully return the spot price for a correct date format" $ \clientEnv -> do
      res <- liftIO $ getSpotPrice clientEnv "BTC" "USD" $ fromGregorianValid 2022 02 01
      res `shouldBe` SpotPriceUnsafe { _base = "BTC", _currency = "USD", _amount = "1232.44" }
