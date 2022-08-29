module Gen (
  address,
  assetClass,
  currencySymbol,
  tokenName,
  pubKeyHash,
  validatorHash,
  datumHash,
  maybeOf,
  rational,
  integer,
  datum,
  txId,
  txOutRef,
  value,
) where

import Apropos (Gen, choice, element, int, linear, list)

import PlutusLedgerApi.V1 (
  Address (Address),
  Credential (..),
  CurrencySymbol,
  Datum (Datum),
  DatumHash,
  PubKeyHash,
  StakingCredential (..),
  TokenName,
  TxId,
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
  singleton,
 )
import PlutusTx.IsData.Class (ToData (toBuiltinData))

import Control.Monad (replicateM)
import Data.String (IsString (..))
import GHC.Real (Ratio ((:%)))
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V1.Value qualified as Value

-- TODO address should get it's own apropos model
address :: Gen Address
address = Address <$> credential <*> maybeOf stakingCredential
  where
    stakingCredential :: Gen StakingCredential
    stakingCredential =
      choice
        [ StakingHash <$> credential
        , StakingPtr <$> integer <*> integer <*> integer
        ]

    credential :: Gen Credential
    credential =
      choice
        [ PubKeyCredential <$> pubKeyHash
        , ScriptCredential <$> validatorHash
        ]

-- TODO review length constraints

hexString :: IsString s => Int -> Int -> Gen s
hexString l u = do
  len <- (2 *) <$> int (linear l u)
  fromString <$> replicateM len hexit

currencySymbol :: Gen CurrencySymbol
currencySymbol = choice [pure "", hexString 32 32]

tokenName :: Gen TokenName
tokenName = hexString 0 32

assetClass :: Gen AssetClass
assetClass = Value.assetClass <$> currencySymbol <*> tokenName

pubKeyHash :: Gen PubKeyHash
pubKeyHash = hexString 32 32

validatorHash :: Gen ValidatorHash
validatorHash = hexString 32 32

datumHash :: Gen DatumHash
datumHash = hexString 32 32

txId :: Gen TxId
txId = hexString 32 32

hexit :: Gen Char
hexit = element $ ['0' .. '9'] ++ ['a' .. 'f']

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = choice [pure Nothing, Just <$> g]

integer :: Gen Integer
integer = toInteger <$> int (linear (-1_000_000) 1_000_000)

pos :: Gen Integer
pos = toInteger <$> int (linear 1 1_000_000)

rational :: Gen Rational
rational = (:%) <$> integer <*> pos

datum :: Gen Datum
datum = choice [datumOf integer, datumOf value]

value :: Gen Value
value = mconcat <$> list (linear 0 64) singletonValue
  where
    singletonValue :: Gen Value
    singletonValue =
      singleton <$> currencySymbol <*> tokenName <*> pos

datumOf :: ToData a => Gen a -> Gen Datum
datumOf g = Datum . toBuiltinData <$> g

txOutRef :: Gen TxOutRef
txOutRef = TxOutRef <$> txId <*> (toInteger <$> int (linear 0 64))
