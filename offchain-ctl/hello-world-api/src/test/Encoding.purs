module Test.Encoding
  ( spec
  ) where


import Contract.Prelude

import CBOR as CBOR
import Test.Spec(Spec,it)
import Data.Log.Level (LogLevel(Trace))
import Serialization.Address (NetworkId(TestnetId))
import Contract.Aeson (decodeAeson, fromString)
import Contract.Scripts (Validator)
import Contract.Monad
  ( Contract
  , ContractConfig(ContractConfig)
  , runContract_
  , configWithLogLevel
  , liftContractM
  )
--import CBOR as CBOR

spec :: Spec Unit
spec = do
  it "hello" $ testCborDecodes CBOR.hello

testCborDecodes :: String -> Aff Unit
testCborDecodes cbor = do
  cfg <- over ContractConfig _ { wallet = Nothing } <$> configWithLogLevel TestnetId undefined Trace
  runContract_ cfg $ do
    decodeCbor $ cbor

decodeCbor :: String -> Contract () Validator
decodeCbor cbor =
  liftContractM "decode failed" $ cbor
    # fromString
    # decodeAeson
    # hush
    # map wrap

