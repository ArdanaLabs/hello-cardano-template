module Test.HelloWorld.Api
  ( spec
  , testnetSpec
  ) where

import Contract.Prelude

import Effect.Exception (throw)
import Contract.Config (testnetConfig)
import Contract.Monad (Contract, ContractEnv, liftContractAffM, runContract, withContractEnv)
import Contract.Scripts (validatorHash)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract, withPlutipContractEnv, runContractInEnv)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.UInt as UInt
import HelloWorld.Api (initialize, increment, redeem, query, helloScript, sendDatumToScript, datumLookup)
import Node.Process (lookupEnv)
import Plutus.Types.Value (Value, valueToCoin, getLovelace)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldReturn, expectError, shouldEqual, shouldSatisfy)
import Util (ourLogger)
import Wallet.Key (KeyWallet, privateKeysToKeyWallet)
import Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Wallet.Spec (WalletSpec(UseKeys), PrivatePaymentKeySource(PrivatePaymentKeyFile), PrivateStakeKeySource(PrivateStakeKeyFile))

type Runner = ((ContractEnv () -> KeyWallet -> Aff Unit) -> Aff Unit)

getRunner :: Aff Runner
getRunner = do
  (liftEffect $ lookupEnv "MODE") >>= case _ of
    Just "local" -> pure $ withPlutipContractEnv config [ BigInt.fromInt 20_000_000 ]
    Just "testnet" -> do
      testResourcesDir <- liftEffect $ fromMaybe "./fixtures/" <$> lookupEnv "TEST_RESOURCES"
      key <- privatePaymentKeyFromFile $ testResourcesDir <> "/wallet.skey"
      stakeKey <- privateStakeKeyFromFile $ testResourcesDir <> "/staking.skey"
      let keyWallet = privateKeysToKeyWallet key (Just stakeKey)
      pure 
        $ \f -> withContractEnv testnetConfig $ \env -> f (env :: ContractEnv ()) (keyWallet :: KeyWallet)
    Just e -> liftEffect $ throw $ "expected local or testnet got: " <> e
    Nothing -> liftEffect $ throw "expected MODE to be set"

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Error
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }

getAmount :: Value -> BigInt.BigInt
getAmount = getLovelace <<< valueToCoin

spec :: Spec Unit
spec = do
  describe "HelloWorld.Api" $ do
    testInitialize
    testIncrement
    testRedeem
    testDatumLookup

testInitialize :: Spec Unit
testInitialize = do
  describe "initialize" do
    it "should set the datum to the initial value" $ do
      let
        initialAdaAmount = BigInt.fromInt 20_000_000
        initialValue = 20
        incParam = 200
      getRunner \env alice -> do
        initOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize incParam initialValue
        (datum /\ value) <-
          runContractInEnv env $
            withKeyWallet alice do
              query initOutput
        datum `shouldEqual` initialValue
        getAmount value `shouldSatisfy` (>) initialAdaAmount
    it "should fail if there isn't enough Ada available"
      $ expectError
      $
        runPlutipContract config [ BigInt.fromInt 1_000_000 ] \alice -> do
          withKeyWallet alice do
            initialize 0 1

testIncrement :: Spec Unit
testIncrement = do
  describe "increment" do
    it "should increment the datum by the specified increment parameter" $ do
      let
        initialValue = 10
        incParam = 2
        distribution = [ BigInt.fromInt 20_000_000 ]
      withPlutipContractEnv config distribution \env alice -> do
        initOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize incParam initialValue
        incOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              increment incParam initOutput
        (datum /\ _) <-
          runContractInEnv env $
            withKeyWallet alice do
              query incOutput
        datum `shouldEqual` (initialValue + incParam)
    it "should fail when providing the wrong increment parameter" $ do
      let
        initialValue = 10
        distribution = [ BigInt.fromInt 20_000_000 ]
      withPlutipContractEnv config distribution \env alice -> do
        lastOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize 2 initialValue
        expectError
          $ runContractInEnv env
          $
            withKeyWallet alice do
              increment 3 lastOutput

testRedeem :: Spec Unit
testRedeem = do
  describe "redeem" do
    it "should return the locked Ada to a wallet" $ do
      let
        initialValue = 20
        incParam = 5
        initialAdaAmountAlice = BigInt.fromInt 20_000_000
        initialAdaAmountBob = BigInt.fromInt 5_000_000
        distribution = [ initialAdaAmountAlice ] /\ [ initialAdaAmountBob ]
      withPlutipContractEnv config distribution \env (alice /\ bob) -> do
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

    it "should succeed after successful initialization and increment" $ do
      let
        initialValue = 20
        incParam = 50
        distribution = [ BigInt.fromInt 20_000_000 ]
      withPlutipContractEnv config distribution \env alice -> do
        initOutput <-
          runContractInEnv env $
            withKeyWallet alice do
              initialize incParam initialValue
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
        datum `shouldEqual` (initialValue + incParam)
        getAmount value `shouldSatisfy` (>) (BigInt.fromInt 20_000_000)

testDatumLookup :: Spec Unit
testDatumLookup = do
  describe "datumLookup"
    $ it "should fetch the correct datum"
    $ do
        let expectedDatum = 3
        datum <- runPlutipContract config [ BigInt.fromInt 20_000_000 ] $ \alice -> do
          withKeyWallet alice do
            validator <- helloScript 1
            vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
            ti1 <- sendDatumToScript expectedDatum vhash
            datumLookup ti1
        datum `shouldEqual` expectedDatum

-- impure testnet tests

testnetSpec :: Spec Unit
testnetSpec = do
  testHelloWorldApiOnTestnet

runContractOnTestnet :: forall a. Contract () a -> Aff a
runContractOnTestnet contract = do
  testResourcesDir <- liftEffect $ fromMaybe "./fixtures/" <$> lookupEnv "TEST_RESOURCES"
  let walletSpec = UseKeys (PrivatePaymentKeyFile $ testResourcesDir <> "/wallet.skey") (Just $ PrivateStakeKeyFile $ testResourcesDir <> "/staking.skey")
  let
    tConfig = testnetConfig
      { walletSpec = Just walletSpec
      , logLevel = Warn
      , customLogger = Just $ ourLogger "./apiTest.log"
      }
  runContract tConfig $ contract

testHelloWorldApiOnTestnet :: Spec Unit
testHelloWorldApiOnTestnet = do
  describe "HelloWorld API" do
    it "should initialize, increment, redeem and query without successfully" $ do
      let
        initialValue = 20
        incParam = 200
      -- initialize
      initOutput <- runContractOnTestnet $ initialize incParam initialValue
      (datum /\ _) <- runContractOnTestnet $ query initOutput
      datum `shouldEqual` initialValue

      -- increment by 200
      incOutput <- runContractOnTestnet $ increment incParam initOutput
      (datum1 /\ _) <- runContractOnTestnet $ query incOutput
      datum1 `shouldEqual` (initialValue + incParam)

      -- increment by another 200
      incOutput2 <- runContractOnTestnet $ increment incParam incOutput
      (datum2 /\ _) <- runContractOnTestnet $ query incOutput2
      datum2 `shouldEqual` (initialValue + (2 * incParam))

      -- redeem, datum should not change
      runContractOnTestnet $ redeem incParam incOutput2 `shouldReturn` unit
      (datum3 /\ _) <- runContractOnTestnet $ query incOutput2
      datum3 `shouldEqual` (initialValue + (2 * incParam))
