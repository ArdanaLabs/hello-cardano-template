{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module PriceData.Types (CurrencyPair(..)) where

import Data.Text qualified as T
import GHC.Generics

data CurrencyPair = CurrencyPair {
  _base :: T.Text
, _currency :: T.Text
} deriving (Eq, Generic, Ord, Show)
