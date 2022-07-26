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

integrationTest :: Int -> Int -> Contract () Unit
integrationTest init param = do
  logInfo' "Full integrationTest"
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript init vhash
  datum1 <- datumLookup ti1
  datum1 `shouldEqual` init
  ti2 <- setDatumAtScript (init + param) vhash validator ti1
  datum2 <- datumLookup ti2
  datum2 `shouldEqual` (init + param)
  redeemFromScript vhash validator ti2

lockTest :: Int -> Int -> Contract () Unit
lockTest init param = do
  logInfo' "Lock test"
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  _ <- sendDatumToScript init vhash
  pure unit

lookupTest :: Int -> Int -> Contract () Int
lookupTest init param = do
  logInfo' "lookup test"
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript init vhash
  datumLookup ti1

incTest :: Int -> Int -> Contract () Unit
incTest init param = do
  logInfo' "inc test"
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript init vhash
  _ti2 <- setDatumAtScript (init + param) vhash validator ti1
  pure unit

postIncLookupTest :: Int -> Int -> Contract () Int
postIncLookupTest init param = do
  logInfo' "inc lookup test"
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript init vhash
  ti2 <- setDatumAtScript (init + param) vhash validator ti1
  datumLookup ti2

unlockTest :: Int -> Int -> Contract () Unit
unlockTest init param = do
  logInfo' "unlock test"
  validator <- helloScript param
  vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
  ti1 <- sendDatumToScript init vhash
  redeemFromScript vhash validator ti1
