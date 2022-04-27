{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module HelloWorld.ValidatorProxy (helloValidator) where

import Data.ByteString.Lazy qualified as BSL
import Codec.Serialise (deserialise)
import Data.FileEmbed (embedFile)
import Plutus.V2.Ledger.Api (Validator(..))

import HelloWorld.ValidatorUtils (getValidatorScriptsPath)

helloValidator :: Validator
helloValidator = Validator . deserialise . BSL.fromStrict $ $(embedFile $ getValidatorScriptsPath ++ "/hello_world.plc")
