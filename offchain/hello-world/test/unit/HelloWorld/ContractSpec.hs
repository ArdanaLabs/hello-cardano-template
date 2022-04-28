module HelloWorld.ContractSpec (testTree, insertValidDataTrace) where

import Data.Functor (void)
import Test.Tasty

import Plutus.Contract.Test
import Plutus.Trace.Emulator

import HelloWorld.Contract (contract)

testTree :: TestTree
testTree = testGroup "HelloWorld Contract Emulator Traces" [
    checkPredicate "updating valid data should not fail" (assertNoFailedTransactions (insertValidDataTrace)
  ]

insertValidDataTrace :: EmulatorTrace ()
insertValidDataTrace = do
  h1 <- activateContractWallet (knownWallet 1) contract
  void $ waitNSlots 1
  callEndpoint @"initialize" h1 1
  void $ waitNSlots 1
  callEndpoint @"increment" h1 ()
  void $ waitNSlots 1
  return ()
