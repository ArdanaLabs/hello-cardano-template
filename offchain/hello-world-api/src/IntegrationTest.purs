module IntegrationTest
  (integrationTest
  ,lockTest
  ,lookupTest
  ,incTest
  ,postIncLookupTest
  ,unlockTest
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
  logInfo' "Full integrationTest"
  validator <- helloScript 4
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript 3 vhash
  datum1 <- datumLookup ti1
  datum1 `shouldEqual` 3
  ti2 <- setDatumAtScript 7 vhash validator ti1
  datum2 <- datumLookup ti2
  datum2 `shouldEqual` 7
  redeemFromScript vhash validator ti2

lockTest :: Contract () Unit
lockTest = do
  logInfo' "Lock test"
  validator <- helloScript 4
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  _ <- sendDatumToScript 3 vhash
  pure unit

lookupTest :: Contract () Int
lookupTest = do
  logInfo' "Full integrationTest"
  validator <- helloScript 4
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript 3 vhash
  datumLookup ti1

incTest :: Contract () Unit
incTest = do
  logInfo' "Full integrationTest"
  validator <- helloScript 4
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript 3 vhash
  _ti2 <- setDatumAtScript 7 vhash validator ti1
  pure unit

postIncLookupTest :: Contract () Int
postIncLookupTest = do
  logInfo' "Full integrationTest"
  validator <- helloScript 4
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript 3 vhash
  ti2 <- setDatumAtScript 7 vhash validator ti1
  datumLookup ti2

unlockTest :: Contract () Unit
unlockTest = do
  logInfo' "Full integrationTest"
  validator <- helloScript 4
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript 3 vhash
  redeemFromScript vhash validator ti1
