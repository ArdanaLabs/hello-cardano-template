{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.ClientSpec (spec) where

import Test.Syd
import Test.Syd.Servant

import Network.Kraken.API (AssetTickerInfoUnsafe(..), marketDataAPIProxy)
import Network.Kraken.Types (TickerData(..))
import Network.Kraken.Client (getAssetTickerInformation, getOHLCData)
import Network.Kraken.Server.Mock (marketDataServer)

spec :: Spec
spec = do
  testGetAssetTickerInformation
  testGetOHLCData

testGetAssetTickerInformation :: Spec
testGetAssetTickerInformation = servantSpec marketDataAPIProxy marketDataServer $ do
  describe "requests with correct credentials" $ do
    it "gets zero at the start" $ \clientEnv -> do
      res <- liftIO $ getAssetTickerInformation clientEnv "hello"
      res `shouldBe` AssetTickerInfoUnsafe
                      ("52609.60000", "1", "1.00")
                      ("52609.50000", "1", "1.00")
                      ("52641.10000", "0.00080000")
                      ("1920.83610601", "7954.00219674")
                      ("52389.94668", "54022.90683")
                      (23329, 80463)
                      ("51513.90000", "51513.90000")
                      ("51513.90000", "51513.90000")
                      "52280.40000"

testGetOHLCData :: Spec
testGetOHLCData = servantSpec marketDataAPIProxy marketDataServer $ do
  describe "testGetOHLCData" $ do
    it "should fetch successfully" $ \clientEnv -> do
      res <- liftIO $ getOHLCData clientEnv "BTC" "USD" Nothing Nothing
      res `shouldBe` [ TickerData 1616662740 52591.9 52599.9 52599.9 52591.8 52599.1 0.11091626 5
                     , TickerData 1616662980 52601.2 52599.9 52601.2 52599.9 52599.9 0.43748934 7
                     ]
