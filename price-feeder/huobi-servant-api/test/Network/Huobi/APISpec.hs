{-# LANGUAGE OverloadedStrings #-}

module Network.Huobi.APISpec (spec) where

import Data.Aeson (eitherDecodeFileStrict)
import Test.Syd

import Network.Huobi.API

spec :: Spec
spec = do
  testParseTick

testParseTick :: Spec
testParseTick = do
  describe "parseJSON Tick" $ do
    it "should succeed parsing the huobi modelled ticker" $ do
      eitherDecodeFileStrict "test-resources/ticker-result.json"
        >>= ( `shouldBe`
                ( Right $
                    TickResponse
                      { _channel = "market.btcusdt.detail.merged"
                      , _status = "ok"
                      , _timestamp = 1629788763750
                      , _tick =
                          Tick
                            { _id = 272156789143
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
                      }
                )
            )
