{-# LANGUAGE OverloadedStrings #-}

module Network.Kucoin.APISpec (spec) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Map
import Test.Syd

import Network.Kucoin.API

spec :: Spec
spec = do
  testParsePrice

testParsePrice :: Spec
testParsePrice = do
  describe "parseJSON FiatPriceResponse" $ do
    it "should succeed parsing the kucoin modelled fiat price" $ do
      eitherDecodeFileStrict "test-resources/fiat-price-response.json"
        >>= (`shouldBe` Right (FiatPriceResponse {_code = "200000", _data = singleton "ADA" "39596.03960396"}))
