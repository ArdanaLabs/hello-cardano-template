{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.ClientSpec (spec) where

import Test.Syd
import Test.Syd.Servant

import Network.Kraken.API (AssetTickerInfoUnsafe(..), marketDataAPIProxy)
import Network.Kraken.Client (getAssetTickerInformation)
import Network.Kraken.Server.Mock (mockTickerInformationHandler)

spec :: Spec
spec = do
  testGetAssetTickerInformation

testGetAssetTickerInformation :: Spec
testGetAssetTickerInformation = servantSpec marketDataAPIProxy mockTickerInformationHandler $ do
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
