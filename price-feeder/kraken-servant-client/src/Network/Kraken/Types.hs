{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Kraken.Types where

import Data.Text as T
import Data.Text.Read (double)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics

data TickerData = TickerData {
  _tickerDataTime :: POSIXTime
, _tickerDataOpen :: Double
, _tickerDataClose :: Double
, _tickerDataHigh :: Double
, _tickerDataLow :: Double
, _tickerDataWeightedAverageVolumePrice :: Double
, _tickerDataVolume :: Double
, _tickerDataCount :: Integer
} deriving (Eq, Generic, Ord, Show)

toTickerData :: (POSIXTime, T.Text, T.Text, T.Text, T.Text, T.Text, T.Text, Integer) -> Either String TickerData
toTickerData (time, openText, highText, lowText, closeText, vwapText, volumeText, _count) =
  TickerData <$> pure time
             <*> eitherDouble openText
             <*> eitherDouble closeText
             <*> eitherDouble highText
             <*> eitherDouble lowText
             <*> eitherDouble vwapText
             <*> eitherDouble volumeText
             <*> pure _count

eitherDouble :: T.Text -> Either String Double
eitherDouble text = do
  res <- double text
  case res of
    (x, "") -> Right x
    (_, remainder) -> Left $ "Unable to read " <> (T.unpack remainder) <> " from input " <> (T.unpack text)