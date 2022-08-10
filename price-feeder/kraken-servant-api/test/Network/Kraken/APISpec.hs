{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Kraken.APISpec (spec) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Map qualified as Map
import Data.Text
import Test.Syd

import Network.Kraken.API

spec :: Spec
spec = do
  testParseKrakenResponse
  testParseAssetTickerInfo
  testParseTickData

testParseKrakenResponse :: Spec
testParseKrakenResponse = do
  describe "parseJSON KrakenResponse" $ do
    it "should succeed for a trivial wrapped type" $ do
      maybeKrakenResponse <- eitherDecodeFileStrict "test-resources/kraken-response-trivial.json"
      maybeKrakenResponse `shouldBe` Right (KrakenResponse [] ("any" :: Text))

testParseAssetTickerInfo :: Spec
testParseAssetTickerInfo = do
  describe "parseJSON AssetTickerInfo" $ do
    it "should succeed parsing the kraken modelled asset ticker info" $ do
      maybeAssetTickerInfo <- eitherDecodeFileStrict "test-resources/asset-ticker-info.json"
      maybeAssetTickerInfo
        `shouldBe` Right
          ( KrakenResponse [] $
              AssetTickerInfoResponse $
                Map.fromList
                  [
                    ( "XXBTZUSD"
                    , AssetTickerInfo
                        ("52609.60000", "1", "1.000")
                        ("52609.50000", "1", "1.000")
                        ("52641.10000", "0.00080000")
                        ("1920.83610601", "7954.00219674")
                        ("52389.94668", "54022.90683")
                        (23329, 80463)
                        ("51513.90000", "51513.90000")
                        ("53219.90000", "57200.00000")
                        "52280.40000"
                    )
                  ]
          )

testParseTickData :: Spec
testParseTickData = do
  describe "parseJSON OHLCResponse" $ do
    it "should succeed parsing the kraken modelled tick data" $ do
      maybeTickData <- eitherDecodeFileStrict "test-resources/tick-data.json"
      maybeTickData
        `shouldBe` Right
          ( KrakenResponse [] $
              OHLCResponse
                { _ohlcResponseLast = 1616662920
                , _ohlcResponsePairs =
                    Map.fromList
                      [
                        ( "XXBTZUSD"
                        ,
                          [ (1616662740, "52591.9", "52599.9", "52591.8", "52599.9", "52599.1", "0.11091626", 5)
                          , (1616662980, "52601.2", "52601.2", "52599.9", "52599.9", "52599.9", "0.43748934", 7)
                          ]
                        )
                      ]
                }
          )
