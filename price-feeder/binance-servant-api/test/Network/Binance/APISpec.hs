{-# LANGUAGE OverloadedStrings #-}

module Network.Binance.APISpec (spec) where

import Data.Aeson (eitherDecodeFileStrict)
import Test.Syd

import Network.Binance.API

spec :: Spec
spec = do
  testParsePrice

testParsePrice :: Spec
testParsePrice = do
  describe "parseJSON PriceResponse" $ do
    it "should succeed parsing the binance modelled price" $ do
      eitherDecodeFileStrict "test-resources/price-result.json"
        >>= (`shouldBe` Right (PriceResponse {_symbol = "ADAUSDT", _price = "0.79260000"}))
