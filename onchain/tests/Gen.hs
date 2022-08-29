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

hexString :: IsString s => Gen s
hexString = do
  len <- (2 *) <$> int (linear 1 32)
  fromString <$> replicateM len hexit

-- specific to tokenName and currencySymbol which can both be only 0 or 64 chars
hexStringName :: IsString s => Gen s
hexStringName = fromString <$> choice [pure "", replicateM 64 hexit]

currencySymbol :: Gen CurrencySymbol
currencySymbol = hexStringName

tokenName :: Gen TokenName
tokenName = hexStringName

assetClass :: Gen AssetClass
assetClass = Value.assetClass <$> currencySymbol <*> tokenName

pubKeyHash :: Gen PubKeyHash
pubKeyHash = hexString

validatorHash :: Gen ValidatorHash
validatorHash = hexString

datumHash :: Gen DatumHash
datumHash = hexString

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
  where
    value :: Gen Value
    value = mconcat <$> list (linear 0 64) singletonValue
    singletonValue :: Gen Value
    singletonValue =
      singleton <$> currencySymbol <*> tokenName <*> pos

datumOf :: ToData a => Gen a -> Gen Datum
datumOf g = Datum . toBuiltinData <$> g
