module Gen (
    address,
    assetClass,
    credential,
    currencySymbol,
    tokenName,
    pubKeyHash,
    validatorHash,
    datumHash,
    value,
    maybeOf,
    stakingCredential,
    rational,
    integer,
    datum,
) where

import Apropos (Gen, choice, element, int, linear, list)

import Plutus.V1.Ledger.Api (
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
import Data.Ratio
import Data.String (IsString (..))
import Plutus.V1.Ledger.Value (AssetClass)
import qualified Plutus.V1.Ledger.Value as Value

address :: Gen Address
address = Address <$> credential <*> maybeOf stakingCredential

credential :: Gen Credential
credential =
    choice
        [ PubKeyCredential <$> pubKeyHash
        , ScriptCredential <$> validatorHash
        ]

hexString :: IsString s => Gen s
hexString = do
    len <- (2 *) <$> int (linear 0 32)
    fromString <$> replicateM len hexit

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

value :: Gen Value
value = mconcat <$> list (linear 0 64) singletonValue

singletonValue :: Gen Value
singletonValue =
    singleton <$> currencySymbol <*> tokenName <*> pos

hexit :: Gen Char
hexit = element $ ['0' .. '9'] ++ ['a' .. 'f']

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = choice [pure Nothing, Just <$> g]

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

rational :: Gen Rational
rational = (%) <$> integer <*> pos

datum :: Gen Datum
datum = choice [datumOf integer, datumOf value]

datumOf :: ToData a => Gen a -> Gen Datum
datumOf g = Datum . toBuiltinData <$> g
