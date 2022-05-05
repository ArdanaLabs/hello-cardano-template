{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Network.Kucoin.ClientSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map as M
import Test.Syd
import Test.Syd.Servant

import Network.Kucoin.API (FiatPriceResponse(..), fiatPriceAPIProxy)
import Network.Kucoin.Client (getFiatPrice)
import Network.Kucoin.Server.Mock (mockFiatPriceHandler)
import PriceData.Types (CurrencyPair(..))
import UnliftIO (tryAny)

spec :: Spec
spec = do
  testGetFiatPrice

testGetFiatPrice :: Spec
testGetFiatPrice = servantSpec fiatPriceAPIProxy mockFiatPriceHandler $ do
  describe "getFiatPrice" $ do
    it "should successfully return the fiat price" $ \clientEnv -> do
      res <- liftIO $ getFiatPrice clientEnv (CurrencyPair "ADA" "USD")
      res `shouldBe` FiatPriceResponse { _code = "200000", _data = M.singleton "ADA" "39596.03960396" }
    it "should throw an error" $ \clientEnv -> do
      eitherErr <- tryAny $ getFiatPrice clientEnv (CurrencyPair "AD" "USDT")
      isLeft eitherErr `shouldBe` True
