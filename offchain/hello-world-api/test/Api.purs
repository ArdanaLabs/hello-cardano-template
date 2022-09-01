module Test.HelloWorld.Api
  ( spec
  , localOnlySpec
  ) where

import Contract.Prelude

import Contract.Monad (liftContractAffM, liftContractM)
import Contract.Scripts (validatorHash)
import Contract.Test.Plutip (runPlutipContract, withPlutipContractEnv, runContractInEnv)
import Contract.Utxos (getWalletBalance)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import HelloWorld.Api (initialize, increment, redeem, query, helloScript, sendDatumToScript, datumLookup)
import Plutus.Types.Value (Value, valueToCoin, getLovelace)
import Test.HelloWorld.EnvRunner (EnvRunner, plutipConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldReturn, expectError, shouldEqual, shouldSatisfy)

getAmount :: Value -> BigInt.BigInt
getAmount = getLovelace <<< valueToCoin

spec :: EnvRunner -> Spec Unit
spec envRunner = do
  describe "HelloWorld.Api" $ do
    traverse_ (\testSpec -> testSpec envRunner)
      [ testInitialize
      , testIncrement
      , testRedeem
      , testDatumLookup
      ]

localOnlySpec :: Spec Unit
localOnlySpec = do
  describe "HelloWorld.Api" do
    testInitializeLocal
    testIncrementLocal
    testRedeemLocal

testInitialize :: EnvRunner -> Spec Unit
testInitialize envRunner = do
  describe "initialize" do
    it "should set the datum to the initial value" $ do
      let
        initialDatum = 20
        incParam = 200
      envRunner \env alice -> do
        initialValue <-
          runContractInEnv env $
            withKeyWallet alice do
              getWalletBalance >>= liftContractM "Get initial wallet balance failed"
        initOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize incParam initialDatum
        (datum /\ value) <-
          runContractInEnv env $
            withKeyWallet alice do
              query initOutput
        datum `shouldEqual` initialDatum
        getAmount value `shouldSatisfy` (>) (getAmount initialValue)

testInitializeLocal :: Spec Unit
testInitializeLocal = do
  describe "initialize" $ do
    it "should fail if there isn't enough Ada available"
      $ expectError
      $
        runPlutipContract plutipConfig [ BigInt.fromInt 1_000_000 ] \alice -> do
          withKeyWallet alice do
            initialize 0 1

testIncrement :: EnvRunner -> Spec Unit
testIncrement envRunner = do
  describe "increment" do
    it "should increment the datum by the specified increment parameter" $ do
      let
        initialDatum = 10
        incParam = 2
      envRunner \env alice -> do
        initOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize incParam initialDatum
        incOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              increment incParam initOutput
        (datum /\ _) <-
          runContractInEnv env $
            withKeyWallet alice do
              query incOutput
        datum `shouldEqual` (initialDatum + incParam)

testIncrementLocal :: Spec Unit
testIncrementLocal = do
  describe "increment" do
    it "should fail when providing the wrong increment parameter" $ do
      let
        initialDatum = 10
        incParam = 2
        wrongIncParam = 23535

      withPlutipContractEnv plutipConfig [ BigInt.fromInt 20_000_000 ] \env alice -> do
        lastOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize incParam initialDatum
        expectError
          $ runContractInEnv env
          $
            withKeyWallet alice do
              increment wrongIncParam lastOutput

testRedeem :: EnvRunner -> Spec Unit
testRedeem envRunner = do
  describe "redeem" do
    it "should succeed after successful initialization and increment" $ do
      let
        initialDatum = 20
        incParam = 50
      envRunner \env alice -> do
        initialValue <-
          runContractInEnv env $
            withKeyWallet alice do
              getWalletBalance >>= liftContractM "Get initial wallet balance failed"
        initOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize incParam initialDatum
        incOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              increment incParam initOutput
        runContractInEnv env $
          withKeyWallet alice do
            redeem incParam incOutput `shouldReturn` unit
        (datum /\ value) <-
          runContractInEnv env $
            withKeyWallet alice do
              query incOutput
        datum `shouldEqual` (initialDatum + incParam)
        getAmount value `shouldSatisfy` (>) (getAmount initialValue)

testRedeemLocal :: Spec Unit
testRedeemLocal = do
  describe "redeem" do
    it "should return the locked Ada to a wallet" $ do
      let
        initialValue = 20
        incParam = 5
        initialAdaAmountAlice = BigInt.fromInt 20_000_000
        initialAdaAmountBob = BigInt.fromInt 5_000_000
        distribution = [ initialAdaAmountAlice ] /\ [ initialAdaAmountBob ]
      withPlutipContractEnv plutipConfig distribution \env (alice /\ bob) -> do
        lastOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize incParam initialValue
        runContractInEnv env $
          withKeyWallet bob do
            redeem incParam lastOutput `shouldReturn` unit
        (_ /\ value) <-
          runContractInEnv env $
            withKeyWallet bob do
              query lastOutput
        -- Bob should have more than at the beginning after redeeming
        getAmount value `shouldSatisfy` (==) initialAdaAmountBob

testDatumLookup :: EnvRunner -> Spec Unit
testDatumLookup envRunner = do
  describe "datumLookup" $
    it "should fetch the correct datum" do
      let expectedDatum = 3
      envRunner \env alice -> do
        datum <- runContractInEnv env $
          withKeyWallet alice do
            validator <- helloScript 1
            vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
            ti1 <- sendDatumToScript expectedDatum vhash
            datumLookup ti1
        datum `shouldEqual` expectedDatum

