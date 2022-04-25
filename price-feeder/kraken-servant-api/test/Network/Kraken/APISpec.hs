{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.APISpec (spec) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Text
import qualified Data.Map as Map
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
      maybeKrakenResponse `shouldBe` (Right $ KrakenResponse [] ("any" :: Text))

testParseAssetTickerInfo :: Spec
testParseAssetTickerInfo = do
  describe "parseJSON AssetTickerInfoUnsafe" $ do
    it "should succeed parsing the kraken modelled asset ticker info" $ do
      maybeAssetTickerInfo <- eitherDecodeFileStrict "test-resources/asset-ticker-info.json"
      maybeAssetTickerInfo `shouldBe` (Right $ KrakenResponse [] $ AssetTickerInfoResponseUnsafe $ Map.fromList [
        ("XXBTZUSD",
          AssetTickerInfoUnsafe
            ("52609.60000", "1", "1.000")
            ("52609.50000", "1", "1.000")
            ("52641.10000", "0.00080000")
            ("1920.83610601", "7954.00219674")
            ("52389.94668", "54022.90683")
            (23329, 80463)
            ("51513.90000", "51513.90000")
            ("53219.90000", "57200.00000")
            "52280.40000"
       )])

testParseTickData :: Spec
testParseTickData = do
  describe "parseJSON TickDataUnsafe" $ do
    it "should succeed parsing the kraken modelled tick data" $ do
      maybeTickData <- eitherDecodeFileStrict "test-resources/tick-data.json"
      maybeTickData `shouldBe` (Right $ KrakenResponse [] $
        OHLCResponseUnsafe { _ohlcResponseUnsafeLast = 1616662920
        , _ohlcResponseUnsafePairs = Map.fromList [
            ("XXBTZUSD", [
              (1616662740, "52591.9", "52599.9", "52591.8", "52599.9", "52599.1", "0.11091626", 5)
            , (1616662980, "52601.2", "52601.2", "52599.9", "52599.9", "52599.9", "0.43748934", 7)
            ]
        )]})
        

-- AssetTickerInfo {
--     _ask = PriceAndVolume 52609.60000 1 1.00
--   , _bid = PriceAndVolume 52609.50000 1 1.00
--   , _lastTradeClosed = (52641.10000, 0.00080000)
--   , _volume = (1920.83610601, 7954.00219674)
--   , _volumeWeightedAveragePrice = (52389.94668, 54022.90683)
--   , _numberOfTrades = (23329, 80463)
--   , _low = (51513.90000, 51513.90000)
--   , _high = (53219.90000, 57200.00000)
--   , _todaysOpeningPrice = 52280.40000
-- }