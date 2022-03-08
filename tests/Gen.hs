module Gen (
  address,
  credential,
  currencySymbol,
  tokenName,
  pubKeyHash,
  validatorHash,
  datumHash,
  value,
  maybeOf,
  stakingCredential,
           ) where

import Apropos ( choice, element, int, list, linear, Gen )

import Plutus.V1.Ledger.Api
    ( CurrencySymbol,
      ValidatorHash,
      Address(Address),
      Credential(..),
      StakingCredential(..),
      PubKeyHash,
      DatumHash,
      TokenName,
      Value,
      singleton )
import Data.String ( IsString(..) )
import Control.Monad ( replicateM )

address :: Gen Address
address = Address <$> credential <*> maybeOf stakingCredential

credential :: Gen Credential
credential =
  choice
    [ PubKeyCredential <$> pubKeyHash
    , ScriptCredential <$> validatorHash
    ]

hexString :: IsString s => Gen s
hexString = fromString <$> list (linear 0 64) hexit

hexStringName :: IsString s => Gen s
hexStringName = fromString <$> choice [ pure "" , replicateM 64 hexit]

currencySymbol :: Gen CurrencySymbol
currencySymbol = hexStringName

tokenName :: Gen TokenName
tokenName = hexStringName

pubKeyHash :: Gen PubKeyHash
pubKeyHash = hexString

validatorHash :: Gen ValidatorHash
validatorHash = hexString

datumHash :: Gen DatumHash
datumHash = hexString

value :: Gen Value
value = mconcat <$> list (linear 0 64) singletonValue

singletonValue :: Gen Value
singletonValue =
  singleton <$> currencySymbol <*> tokenName <*> pos

hexit :: Gen Char
hexit = element $ ['0'..'9'] ++ ['a'..'f']

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = choice [pure Nothing,Just <$> g]

stakingCredential :: Gen StakingCredential
stakingCredential =
  choice
    [ StakingHash <$> credential
    , StakingPtr <$> integer <*> integer <*> integer
    ]

integer :: Gen Integer
integer = fromIntegral <$> int (linear (-1_000_000) 1_000_000)

pos :: Gen Integer
pos = fromIntegral <$> int (linear 1 1_000_000)
