module HelloWorld.Test.E2E.TestPlans.NamiWallet where

import Contract.Prelude

import Control.Parallel (parTraverse_)
import HelloWorld.Test.E2E.Constants as Constants
import HelloWorld.Test.E2E.TestPlans.NamiWallet.Increment as TestPlanIncrement
import HelloWorld.Test.E2E.TestPlans.NamiWallet.Initialize as TestPlanInitialize
import HelloWorld.Test.E2E.TestPlans.NamiWallet.Redeem as TestPlanRedeem
import HelloWorld.Test.E2E.NamiWallet (mkTestOptions, testWallet1, testWallet2, testWallet3, topup)
import Mote (group)
import Test.Spec.Runner as SpecRunner
import Utils as Utils

runTestPlans :: Aff Unit
runTestPlans = do
  wallet1 <- liftEffect testWallet1
  wallet2 <- liftEffect testWallet2
  wallet3 <- liftEffect testWallet3

  parTraverse_ (liftEffect <<< topup) [ wallet1, wallet2, wallet3 ]

  testOptions1 <- liftEffect $ mkTestOptions wallet1
  testOptions2 <- liftEffect $ mkTestOptions wallet2
  testOptions3 <- liftEffect $ mkTestOptions wallet3

  Utils.interpret'
    (SpecRunner.defaultConfig { timeout = pure $ wrap Constants.specRunnerTimeoutMs })
    ( group "Nami wallet" do
        TestPlanInitialize.testPlan testOptions1
        TestPlanIncrement.testPlan testOptions2
        TestPlanRedeem.testPlan testOptions3
    )