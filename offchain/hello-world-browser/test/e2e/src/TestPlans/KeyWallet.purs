module HelloWorld.Test.E2E.TestPlans.KeyWallet where

import Contract.Prelude

import HelloWorld.Test.E2E.Constants as Constants
import HelloWorld.Test.E2E.KeyWallet (mkTestOptions)
import HelloWorld.Test.E2E.TestPlans.KeyWallet.Increment as TestPlanIncrement
import HelloWorld.Test.E2E.TestPlans.KeyWallet.Initialize as TestPlanInitialize
import HelloWorld.Test.E2E.TestPlans.KeyWallet.Redeem as TestPlanRedeem
import Mote (group)
import Test.Spec.Runner as SpecRunner
import Utils as Utils

runTestPlans :: Aff Unit
runTestPlans = do
  testOptions1 <- liftEffect mkTestOptions
  testOptions2 <- liftEffect mkTestOptions
  testOptions3 <- liftEffect mkTestOptions
  Utils.interpret'
    (SpecRunner.defaultConfig { timeout = pure $ wrap Constants.specRunnerTimeoutMs })
    ( group "Key wallet" do
        TestPlanInitialize.testPlan testOptions1
        TestPlanIncrement.testPlan testOptions2
        TestPlanRedeem.testPlan testOptions3
    )
    Utils.InSequence