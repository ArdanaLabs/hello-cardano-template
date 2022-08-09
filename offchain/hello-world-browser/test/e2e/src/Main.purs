module HelloWorld.Test.E2E.Main where

import Prelude

import Control.Parallel (parTraverse_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import HelloWorld.Test.E2E.Constants as Constants
import HelloWorld.Test.E2E.Env as Env
import HelloWorld.Test.E2E.Helpers (closeStaticServer, startStaticServer)
import HelloWorld.Test.E2E.TestPlans.Increment as TestPlanIncrement
import HelloWorld.Test.E2E.TestPlans.Initialize as TestPlanInitialize
import HelloWorld.Test.E2E.TestPlans.Redeem as TestPlanRedeem
import HelloWorld.Test.E2E.TestWallet (mkTestOptions, testWallet1, testWallet2, testWallet3, topup)
import Mote (group)
import Node.Process (lookupEnv)
import Test.Spec.Runner as SpecRunner
import Utils as Utils

main ∷ Effect Unit
main = do
  noRuntime <- isJust <$> lookupEnv Env.noRuntime
  if noRuntime then
    log "skip the test since there's no ctl-runtime"
  else do
    helloWorldBrowserIndex <- lookupEnv Env.helloWorldBrowserIndex

    case helloWorldBrowserIndex of
      Nothing -> throw "HELLO_WORLD_BROWSER_INDEX not set"
      Just index ->
        launchAff_ do
          bracket (startStaticServer index) closeStaticServer $ \_ -> do
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
