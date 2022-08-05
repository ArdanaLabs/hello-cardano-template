module HelloWorld.Test.E2E.Main where

import Prelude

import Contract.Test.E2E (RunningExample(..), TestOptions, WalletExt(..), delaySec, namiConfirmAccess)
import Control.Parallel (parTraverse_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (wrap)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import HelloWorld.Test.E2E.Env as Env
import HelloWorld.Test.E2E.Helpers (clickButton, closeStaticServer, getCurrentValueBody, getCurrentValueHeader, getFundsLockedBody, getFundsLockedHeader, injectJQuery, namiSign', readString, runE2ETest, startStaticServer)
import HelloWorld.Test.E2E.TestWallet (mkTestOptions, testWallet1, testWallet2, testWallet3)
import Mote (group)
import Node.Process (lookupEnv)
import Test.Spec.Runner as SpecRunner
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T
import Utils as Utils

timeoutMs :: Int
timeoutMs = 240_000_000

threeSeconds :: Number
threeSeconds = 3.0

specRunnerTimeoutMs :: Number
specRunnerTimeoutMs = 500_000.0

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
            testOptions1 <- liftEffect $ testWallet1 >>= mkTestOptions
            testOptions2 <- liftEffect $ testWallet2 >>= mkTestOptions
            testOptions3 <- liftEffect $ testWallet3 >>= mkTestOptions

            parTraverse_
              ( \testPlanM -> do
                  Utils.interpret'
                    (SpecRunner.defaultConfig { timeout = pure $ wrap specRunnerTimeoutMs })
                    testPlanM
              )
              [ testPlan_Initialize testOptions1, testPlan_Increment testOptions2, testPlan_Redeem testOptions3 ]

testPlan_Initialize :: TestOptions -> TestPlanM Unit
testPlan_Initialize testOptions = group "When initialize button is clicked" do
  runE2ETest "It shows loading dialog" testOptions NamiExt $ \(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page
    content <- T.content page
    Assert.assert "Initializing" (contains (Pattern "Initializing ...") content)

  runE2ETest "It locks ADA at contract address" testOptions NamiExt $ \example@(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    namiConfirmAccess example

    delaySec threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#current-value-header") { timeout: timeoutMs } page
    currentValueHeaderContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueHeader page
    Assert.assert "Current value header" (contains (Pattern "Current Value") currentValueHeaderContent)

    void $ T.pageWaitForSelector (T.Selector "#current-value-body") { timeout: timeoutMs } page
    currentValueBodyContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueBody page
    Assert.assert "Current value body" (contains (Pattern "3") currentValueBodyContent)

    void $ T.pageWaitForSelector (T.Selector "#funds-locked-header") { timeout: timeoutMs } page
    fundsLockedHeaderContent <- readString <$> T.unsafeEvaluateStringFunction getFundsLockedHeader page
    Assert.assert "Funds locked header" (contains (Pattern "Funds Locked") fundsLockedHeaderContent)

    void $ T.pageWaitForSelector (T.Selector "#funds-locked-body") { timeout: timeoutMs } page
    fundsLockedBodyContent <- readString <$> T.unsafeEvaluateStringFunction getFundsLockedBody page
    Assert.assert "Funds locked body" (contains (Pattern "10.0 ADA") fundsLockedBodyContent)

testPlan_Increment :: TestOptions -> TestPlanM Unit
testPlan_Increment testOptions = group "When increment button is clicked" do
  runE2ETest "It shows loading dialog" testOptions NamiExt $ \example@(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    namiConfirmAccess example

    delaySec threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#increment") { timeout: timeoutMs } page
    clickButton "+" page

    content <- T.content page
    Assert.assert "Incrementing" (contains (Pattern "Incrementing from 3 to 5 ...") content)

  runE2ETest "It increments the datum at script address by 2" testOptions NamiExt $ \example@(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    namiConfirmAccess example

    delaySec threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#increment") { timeout: timeoutMs } page
    clickButton "+" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#current-value-body") { timeout: timeoutMs } page
    currentValueBodyContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueBody page
    Assert.assert "Current value body" (contains (Pattern "5") currentValueBodyContent)

testPlan_Redeem :: TestOptions -> TestPlanM Unit
testPlan_Redeem testOptions = group "When redeem button is clicked" do
  runE2ETest "It shows loading dialogue" testOptions NamiExt $ \example@(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    namiConfirmAccess example

    delaySec threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: timeoutMs } page
    clickButton "Redeem" page

    content <- T.content page
    Assert.assert "Redeeming" (contains (Pattern "Redeeming 10.0 ADA ...") content)

  runE2ETest "It goes to initial page" testOptions NamiExt $ \example@(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    namiConfirmAccess example

    delaySec threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: timeoutMs } page
    clickButton "Redeem" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: timeoutMs } page

