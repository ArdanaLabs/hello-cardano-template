module HelloWorld.ContractSpec (testTree, initializeHelloWorldDatum, incrementHelloWorldDatum, readHelloWorldDatum) where

import Data.Functor (void)
import Test.Tasty

import Plutus.Contract.Test
import Plutus.V1.Ledger.Api (fromBuiltinData)
import Plutus.Trace.Emulator

import HelloWorld.ValidatorProxy (helloValidatorAddress)
import HelloWorld.Contract (contract)

testTree :: TestTree
testTree = testGroup "HelloWorld Contract Emulator Traces" [
    checkPredicate "should initialize correctly" (assertNoFailedTransactions .&&. assertHelloWorldDatumEquals 1) initializeHelloWorldDatum
  , checkPredicate "should increment once" (assertNoFailedTransactions .&&. assertHelloWorldDatumEquals 2) incrementHelloWorldDatum
  ]

assertHelloWorldDatumEquals :: Integer -> TracePredicate
assertHelloWorldDatumEquals expected = dataAtAddress helloValidatorAddress $ \ ds ->
  any ((Just expected ==) . fromBuiltinData) ds

initializeHelloWorldDatum :: EmulatorTrace ()
initializeHelloWorldDatum = do
  h1 <- activateContractWallet (knownWallet 1) contract
  void $ waitNSlots 1
  callEndpoint @"initialize" h1 1
  void $ waitNSlots 1

incrementHelloWorldDatum :: EmulatorTrace ()
incrementHelloWorldDatum = do
  h1 <- activateContractWallet (knownWallet 1) contract
  void $ waitNSlots 1
  callEndpoint @"initialize" h1 1
  void $ waitNSlots 1
  callEndpoint @"increment" h1 ()
  void $ waitNSlots 1

readHelloWorldDatum :: EmulatorTrace ()
readHelloWorldDatum = do
  h1 <- activateContractWallet (knownWallet 1) contract
  void $ waitNSlots 1
  callEndpoint @"initialize" h1 1
  void $ waitNSlots 1
  callEndpoint @"increment" h1 ()
  void $ waitNSlots 1
