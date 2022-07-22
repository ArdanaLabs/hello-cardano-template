module IntegrationTest
  (integrationTest
  ) where

import Contract.Prelude

import Api
  (helloScript
  ,sendDatumToScript
  ,setDatumAtScript
  ,redeemFromScript
  ,datumLookup
  )

import Contract.Monad (Contract, liftContractAffM, logInfo')
import Contract.Scripts (validatorHash)
import Test.Spec.Assertions(shouldEqual)

integrationTest :: Contract () Unit
integrationTest = do
  logInfo' "Running Examples.Hello"
  validator <- helloScript 4
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript 3 vhash
  datum1 <- datumLookup ti1
  datum1 `shouldEqual` 3
  ti2 <- setDatumAtScript 7 vhash validator ti1
  datum2 <- datumLookup ti2
  datum2 `shouldEqual` 7
  redeemFromScript vhash validator ti2
