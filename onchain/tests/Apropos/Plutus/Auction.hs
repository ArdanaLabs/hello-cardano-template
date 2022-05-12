module Apropos.Plutus.Auction (spec) where

import Apropos
import Gen
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (AssetClass, assetClassValue)
import Test.Syd
import Test.Syd.Hedgehog (fromHedgehogGroup)

spec :: Spec
spec = do
  xdescribe "auction model" $ do
    fromHedgehogGroup $ runGeneratorTestsWhere @AuctionProp "generator" Yes

data AuctionProp
  = Valid
  -- TODO real properties
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Enumerable)

data AuctionModel = AuctionModel
  { auction :: TxOut
  , price :: Rational
  , buying :: AssetClass
  , selling :: AssetClass
  }
  deriving stock (Eq, Show)

instance LogicalModel AuctionProp where
  logic = Var Valid

instance HasLogicalModel AuctionProp AuctionModel where
  satisfiesProperty _ _ = True

instance HasParameterisedGenerator AuctionProp AuctionModel where
  parameterisedGenerator s = do
    let _var p = p `elem` s
    adr <- address
    buying <- assetClass
    selling <- genFilter (/= buying) assetClass
    price <- rational
    amt <- integer
    pure $
      AuctionModel
        { -- TODO correct datum
          auction = TxOut adr (assetClassValue selling amt) Nothing
        , price = price
        , buying = buying
        , selling = selling
        }
