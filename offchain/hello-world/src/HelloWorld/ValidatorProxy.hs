{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module HelloWorld.ValidatorProxy (helloValidator, helloValidatorAddress, helloValidatorHash) where

import Data.ByteString.Lazy qualified as BSL
import Codec.Serialise (deserialise)
import Data.FileEmbed (embedFile)
import Ledger (Address(..), ValidatorHash, scriptAddress, validatorHash)
import Plutus.V2.Ledger.Api (Validator(..))

import HelloWorld.ValidatorUtils (getValidatorScriptsPath)

helloValidatorAddress :: Address
helloValidatorAddress = scriptAddress helloValidator

helloValidatorHash :: ValidatorHash
helloValidatorHash = validatorHash helloValidator

helloValidator :: Validator
helloValidator = Validator . deserialise . BSL.fromStrict $ $(embedFile $ getValidatorScriptsPath ++ "/hello_world.plc")
