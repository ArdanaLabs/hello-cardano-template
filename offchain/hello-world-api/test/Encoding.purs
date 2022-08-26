module Test.Encoding
  ( spec
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Monad (Contract, runContractInEnv)
import Contract.Scripts (Validator(Validator))
import Contract.Test.Plutip (PlutipConfig, withKeyWallet, withPlutipContractEnv)
import Contract.TextEnvelope (textEnvelopeBytes, TextEnvelopeType(PlutusScriptV2))
import Data.BigInt as BigInt
import Data.UInt as UInt
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual,shouldNotEqual)
import Types.Scripts (plutusV2Script)
import Util (withOurLogger, decodeCbor)

spec :: Spec Unit
spec = describe "decodeCbor" $ do
  it "fails on invalid" $
    decodeCbor "invalid" `shouldEqual` Nothing
  it "hello decodes" $
    decodeCbor CBOR.hello `shouldNotEqual` Nothing

-- TODO shouldSatisify isJust would be better
-- as it wouldn't needlesly require (Eq a)
-- and imo is more readable
-- but shouldSatisify fails to import for some reason
