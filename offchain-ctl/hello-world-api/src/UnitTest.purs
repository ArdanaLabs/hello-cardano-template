module UnitTest
  (helloUnitTest
  ) where

import Contract.Prelude

import Api(helloScript, sendDatumToScript, setDatumAtScript, redeemFromScript)

import Contract.Monad (Contract, liftContractM, logInfo')
import Contract.Scripts (validatorHash)

helloUnitTest :: Contract () Unit
helloUnitTest = do
  logInfo' "Running Examples.Hello"
  validator <- helloScript 4
  vhash <- liftContractM "Couldn't hash validator" $ validatorHash validator
  sendDatumToScript 5 vhash >>= setDatumAtScript 9 vhash validator >>= redeemFromScript vhash validator

