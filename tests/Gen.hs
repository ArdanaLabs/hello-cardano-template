module Gen (
  address,
  credential,
  pubKeyHash,
  validatorHash,
  datumHash,
  value,
  maybeOf,
  stakingCredential,
           ) where

import Apropos

import Plutus.V1.Ledger.Api
import Data.String

address :: Gen Address
address = Address <$> credential <*> maybeOf stakingCredential

credential :: Gen Credential
credential =
  choice
    [ PubKeyCredential <$> pubKeyHash
    , ScriptCredential <$> validatorHash
    ]

pubKeyHash :: Gen PubKeyHash
pubKeyHash = hexString

validatorHash :: Gen ValidatorHash
validatorHash = hexString

datumHash :: Gen DatumHash
datumHash = hexString

value :: Gen Value
value = undefined

hexString :: IsString s => Gen s
hexString = fromString <$> list (linear 0 64) hexit

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
