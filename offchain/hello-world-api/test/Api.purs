module Test.HelloWorld.Api
  ( spec
  , localOnlySpec
  ) where

import Contract.Prelude

import Contract.Monad (ContractEnv, liftContractM)
import Contract.Scripts (validatorHash)
import Contract.Test.Plutip (runPlutipContract, withPlutipContractEnv, runContractInEnv)
import Contract.Utxos (getWalletBalance)
import Contract.Value (Value, getLovelace, valueToCoin)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import HelloWorld.Api (initialize, increment, redeem, query, helloScript, sendDatumToScript, datumLookup)
import Test.HelloWorld.EnvRunner (EnvRunner, plutipConfig, runEnvSpec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldReturn, expectError, shouldEqual, shouldSatisfy)
import Util (withOurLogger)

getAmount :: Value -> BigInt.BigInt
getAmount = getLovelace <<< valueToCoin

spec :: EnvRunner -> Spec Unit
spec = runEnvSpec do
  describe "HelloWorld.Api" $ do
    describe "initialize" do
      it "should set the datum to the initial value" $
        \(envRunner :: EnvRunner) -> do
          let
            initialDatum = 20
            incParam = 200
          envRunner \env alice -> do
            initialValue <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  getWalletBalance >>= liftContractM "Get initial wallet balance failed"
            initOutput <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  initialize incParam initialDatum
            (datum /\ value) <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  query initOutput
            datum `shouldEqual` initialDatum
            getAmount value `shouldSatisfy` (>) (getAmount initialValue)

    describe "increment" do
      it "should increment the datum by the specified increment parameter" $
        \(envRunner :: EnvRunner) -> do
          let
            initialDatum = 10
            incParam = 2
          envRunner \env alice -> do
            initOutput <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  initialize incParam initialDatum
            incOutput <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  increment incParam initOutput
            (datum /\ _) <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  query incOutput
            datum `shouldEqual` (initialDatum + incParam)

    describe "redeem" do
      it "should succeed after successful initialization and increment" $
        \(envRunner :: EnvRunner) -> do
          let
            initialDatum = 20
            incParam = 50
          envRunner \env alice -> do
            initialValue <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  getWalletBalance >>= liftContractM "Get initial wallet balance failed"
            initOutput <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  initialize incParam initialDatum
            incOutput <-
              runContractInEnv (withApiLogger env) $
                withKeyWallet alice do
                  increment incParam initOutput
            runContractInEnv (withApiLogger env) $
              withKeyWallet alice do
                redeem incParam incOutput `shouldReturn` unit
            value <- runContractInEnv (withApiLogger env)
              $ withKeyWallet alice
              $ getWalletBalance
              >>= liftContractM "get wallet ballance failed"
            getAmount value `shouldSatisfy` (>) (getAmount initialValue)

    describe "datumLookup"
      $ it "should fetch the correct datum"
      $
        \(envRunner :: EnvRunner) -> do
          let expectedDatum = 3
          envRunner \env alice -> do
            datum <- runContractInEnv (withApiLogger env) $
              withKeyWallet alice do
                validator <- helloScript 1
                let vhash = validatorHash validator
                ti1 <- sendDatumToScript expectedDatum vhash
                datumLookup ti1
            datum `shouldEqual` expectedDatum

withApiLogger :: ContractEnv () -> ContractEnv ()
withApiLogger = withOurLogger "apiTest.log"

localOnlySpec :: Spec Unit
localOnlySpec = do
  describe "HelloWorld.Api" do
    describe "initialize" $ do
      it "should fail if there isn't enough Ada available"
        $ expectError
        $
          runPlutipContract plutipConfig [ BigInt.fromInt 1_000_000 ] \alice -> do
            withKeyWallet alice do
              initialize 0 1

    describe "increment" do
      it "should fail when providing the wrong increment parameter" $ do
        let
          initialDatum = 10
          incParam = 2
          wrongIncParam = 23535

        withPlutipContractEnv plutipConfig [ BigInt.fromInt 20_000_000 ] \env alice -> do
          lastOutput <-
            runContractInEnv (withApiLogger env) $
              withKeyWallet alice do
                initialize incParam initialDatum
          expectError
            $ runContractInEnv (withApiLogger env)
            $
              withKeyWallet alice do
                increment wrongIncParam lastOutput

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
            runContractInEnv (withApiLogger env) $
              withKeyWallet alice do
                initialize incParam initialValue
          runContractInEnv (withApiLogger env) $
            withKeyWallet bob do
              redeem incParam lastOutput `shouldReturn` unit
          value <-
            runContractInEnv (withApiLogger env)
              $ withKeyWallet bob
              $ getWalletBalance
              >>= liftContractM "get wallet balance failed"

          -- Bob should have more than at the beginning after redeeming
          getAmount value `shouldSatisfy` (_ >= initialAdaAmountBob)
