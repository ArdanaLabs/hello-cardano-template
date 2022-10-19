module HelloWorld.Discovery.Types
  ( Vault(Vault)
  , Protocol
  , VaultId
  , HelloRedeemer(HelloRedeemer)
  , HelloAction(Inc, Spend)
  , NftRedeemer(NftRedeemer, Burning)
  ) where

import Contract.Prelude

import Contract.Address (PubKeyHash)
import Contract.PlutusData (class ToData, class FromData, PlutusData(..), fromData, toData)
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction (TransactionInput)
import Contract.Value (TokenName)
import Control.Alternative (guard)
import Data.BigInt as BigInt
import Data.UInt as UInt

type Protocol = { config :: TransactionInput, vaultValidator :: Validator, nftMp :: MintingPolicy }

type VaultId = TokenName

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
    guard $ n == zero
    owner <- fromData owner'
    count <- fromData count'
    pure $ Vault { owner, count }
  fromData _ = Nothing

instance ToData HelloAction where
  toData Inc = Constr zero []
  toData Spend = Constr one []

instance ToData HelloRedeemer where
  toData (HelloRedeemer { tn, action }) = Constr zero [ toData tn, toData action ]

instance ToData NftRedeemer where
  toData (NftRedeemer { tn, txId }) = Constr zero [ toData tn, Constr zero [ Constr zero [ Bytes $ unwrap (unwrap txId).transactionId ], Integer $ BigInt.fromInt $ UInt.toInt (unwrap txId).index ] ]
  toData Burning = Constr one []
