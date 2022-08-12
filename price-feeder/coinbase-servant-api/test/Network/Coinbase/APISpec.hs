{-# LANGUAGE OverloadedStrings #-}

module Network.Coinbase.APISpec (spec) where

import Data.Aeson (eitherDecodeFileStrict)
import Test.Syd

import Network.Coinbase.API

spec :: Spec
spec = do
  testParseCoinbaseResponse
  testParseSpotPrice

testParseCoinbaseResponse :: Spec
testParseCoinbaseResponse = do
  describe "parseJSON CoinbaseResponse" $ do
    it "should succeed for a trivial wrapped type" $ do
      eitherDecodeFileStrict "test-resources/coinbase-trivial-response.json"
        >>= (`shouldBe` Right (CoinbaseResponse Nothing $ Just (1 :: Integer)))

testParseSpotPrice :: Spec
testParseSpotPrice = do
  describe "parseJSON SpotPrice" $ do
    it "should succeed parsing the coinbase modelled spot price" $ do
      eitherDecodeFileStrict "test-resources/spot-price-result.json"
        >>= (`shouldBe` Right (CoinbaseResponse Nothing $ Just $ SpotPrice {_base = "BTC", _currency = "USD", _amount = "41397.58"}))
    it "should succeed parsing the coinbase modelled error" $ do
      eitherDecodeFileStrict "test-resources/spot-price-error-result.json"
        >>= (`shouldBe` Right (CoinbaseResponse (Just [CoinbaseError "invalid_request" "Invalid date (2022/01/01)- use format YYYY-MM-DD"]) Nothing :: CoinbaseResponse SpotPrice))
