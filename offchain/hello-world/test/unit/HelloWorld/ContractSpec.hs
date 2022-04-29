module HelloWorld.ContractSpec (testTree, initializeHelloWorldDatum, incrementHelloWorldDatum, readHelloWorldDatum) where

import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Monoid (Last(..))
import Test.Tasty

import Plutus.Contract.Test
import Plutus.V1.Ledger.Api (fromBuiltinData)
import Plutus.Trace.Emulator

import HelloWorld.ValidatorProxy (helloValidatorAddress)
import HelloWorld.Contract (initialize, increment, read')

testTree :: TestTree
testTree = testGroup "HelloWorld Contract Emulator Traces" [
    checkPredicate "should initialize correctly" (assertNoFailedTransactions .&&. assertHelloWorldDatumEquals 1) initializeHelloWorldDatum
  , checkPredicate "should increment once" (assertNoFailedTransactions .&&. assertHelloWorldDatumEquals 2) incrementHelloWorldDatum
  , checkPredicate "should read successfully" (assertNoFailedTransactions
                                          .&&. assertHelloWorldDatumEquals 2
                                          .&&. assertAccumState read' (walletInstanceTag $ knownWallet 3) (== (Last $ Just 2)) "hehe")
                                          incrementHelloWorldDatum
  ]

assertHelloWorldDatumEquals :: Integer -> TracePredicate
assertHelloWorldDatumEquals expected = dataAtAddress helloValidatorAddress $ \ ds ->
  any ((Just expected ==) . fromBuiltinData) ds

initializeHelloWorldDatum :: EmulatorTrace ()
initializeHelloWorldDatum = do
  h1 <- activateContractWallet (knownWallet 1) initialize
  void $ waitNSlots 1
  callEndpoint @"initialize" h1 1
  void $ waitNSlots 1

incrementHelloWorldDatum :: EmulatorTrace ()
incrementHelloWorldDatum = do
  h1 <- activateContractWallet (knownWallet 1) initialize
  void $ waitNSlots 1
  callEndpoint @"initialize" h1 1
  void $ waitNSlots 1
  h2 <- activateContractWallet (knownWallet 2) increment
  void $ waitNSlots 1
  callEndpoint @"increment" h2 ()
  void $ waitNSlots 1

readHelloWorldDatum :: EmulatorTrace ()
readHelloWorldDatum = do
  h1 <- activateContractWallet (knownWallet 1) initialize
  void $ waitNSlots 1
  callEndpoint @"initialize" h1 1
  void $ waitNSlots 1
  h2 <- activateContractWallet (knownWallet 2) increment
  void $ waitNSlots 1
  callEndpoint @"increment" h2 ()
  void $ waitNSlots 1
  h3 <- activateContractWallet (knownWallet 3) read'
  void $ waitNSlots 1
  callEndpoint @"read" h3 ()
  _ <- fromJust . getLast <$> observableState h3
  void $ waitNSlots 1

