module HelloWorld.Discovery.Types
  ( Vault(Vault)
  , Protocol
  , HelloRedeemer(HelloRedeemer)
  , HelloAction(Inc, Spend)
  , NftRedeemer(NftRedeemer, Burning)
  ) where

import Contract.Prelude
import Data.BigInt as BigInt
import Data.UInt as UInt

import Contract.Scripts (Validator)
import Control.Alternative (guard)
import Contract.Transaction (TransactionInput)
import Contract.PlutusData (PlutusData(..), class FromData, fromData)
import Contract.Value (TokenName)
import Types.PubKeyHash (PubKeyHash)
import Types.Scripts (MintingPolicy)
import ToData (class ToData, toData)

type Protocol = { config :: TransactionInput, vaultValidator :: Validator, nftMp :: MintingPolicy }

newtype Vault = Vault { owner :: PubKeyHash, count :: BigInt.BigInt }

derive instance Newtype Vault _

data HelloRedeemer = HelloRedeemer { tn :: TokenName, action :: HelloAction }

data HelloAction = Inc | Spend

data NftRedeemer
  = NftRedeemer { tn :: TokenName, txId :: TransactionInput }
  | Burning

-- I'm not using generics in this module because
-- frankly it's worse than writing them by hand

instance ToData Vault where
  toData (Vault { owner, count }) = Constr zero [ toData owner, Integer count ]

instance FromData Vault where
  fromData (Constr n [ owner', count' ]) = do
    guard $ n == BigInt.fromInt 0
    owner <- fromData owner'
    count <- fromData count'
    pure $ Vault { owner, count }
  fromData _ = Nothing

instance ToData HelloAction where
  toData Inc = Constr zero []
  toData Spend = Constr zero []

instance ToData HelloRedeemer where
  toData (HelloRedeemer { tn, action }) = Constr zero [ toData tn, toData action ]

instance ToData NftRedeemer where
  toData (NftRedeemer { tn, txId }) = Constr zero [ toData tn, Constr zero [ Constr zero [ Bytes $ unwrap (unwrap txId).transactionId ], Integer $ BigInt.fromInt $ UInt.toInt (unwrap txId).index ] ]
  toData Burning = Constr one []
