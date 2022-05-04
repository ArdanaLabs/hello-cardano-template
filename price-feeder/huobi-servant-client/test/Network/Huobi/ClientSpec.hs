{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Network.Huobi.ClientSpec (spec) where

import Data.Either (isLeft)
import Test.Syd
import Test.Syd.Servant

import Network.Huobi.API (Tick(..), marketDataAPIProxy)
import Network.Huobi.Client (getTick)
import Network.Huobi.Server.Mock (mockGetTick)
import PriceData.Types (CurrencyPair(..))
import UnliftIO (tryAny)

spec :: Spec
spec = do
  testGetTick

testGetTick :: Spec
testGetTick = servantSpec marketDataAPIProxy mockGetTick $ do
  describe "getTick" $ do
    it "should successfully return the tick" $ \clientEnv -> do
      res <- liftIO $ getTick clientEnv (CurrencyPair "ADA" "USDT")
      res `shouldBe` Tick {
                                                    _id = 272156789143
                                                  , _version = 272156789143
                                                  , _open = 50080.0
                                                  , _close = 49820.92
                                                  , _low = 48767.0
                                                  , _high = 50500.0
                                                  , _amount = 12055.365781937457
                                                  , _vol = 5.985618685709001E8
                                                  , _count = 420573
                                                  , _bid = (49819.48, 2.58112)
                                                  , _ask = (49819.49, 0.002411)
                                                  }
    it "should throw an error" $ \clientEnv -> do
      eitherErr <- tryAny $ getTick clientEnv (CurrencyPair "AD" "USDT")
      isLeft eitherErr `shouldBe` True
