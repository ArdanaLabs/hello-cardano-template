module HelloWorld.ContractSpec (testTree) where

import Data.Functor (void)
import Test.Tasty

import Plutus.Contract.Test
import Plutus.Trace.Emulator

import HelloWorld.Contract (contract)

testTree = testGroup "HelloWorld Contract Emulator Traces" [
    checkPredicate "updating valid data should not fail" assertNoFailedTransactions (insertValidDataTrace 1)
  ]

insertValidDataTrace :: Integer -> EmulatorTrace ()
insertValidDataTrace updaterWalletId = do
  h1 <- activateContractWallet (knownWallet 1) contract
  void $ waitNSlots 1
  callEndpoint @"initialize" h1 0
  return ()
