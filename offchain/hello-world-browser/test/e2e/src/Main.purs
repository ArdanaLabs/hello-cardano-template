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
import Foreign (Foreign)
import Foreign as Foreign
import HelloWorld.Test.E2E.Env as Env
import HelloWorld.Test.E2E.Helpers (clickButton, closeStaticServer, injectJQuery, mkTestOptions, namiSign', runE2ETest, startStaticServer)
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
          bracket (startStaticServer index) closeStaticServer $ \_ ->
            parTraverse_
              ( \testPlan -> do
                  testOptions <- liftEffect mkTestOptions
                  Utils.interpret'
                    (SpecRunner.defaultConfig { timeout = pure $ wrap specRunnerTimeoutMs })
                    (testPlan testOptions)
              )
              [ testPlan_Initialize, testPlan_Increment, testPlan_Redeem ]

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

getCurrentValueHeader :: String
getCurrentValueHeader = "[].map.call(document.querySelectorAll('#current-value-header'), el => el.textContent)"

getCurrentValueBody :: String
getCurrentValueBody = "[].map.call(document.querySelectorAll('#current-value-body'), el => el.textContent)"

getFundsLockedHeader :: String
getFundsLockedHeader = "[].map.call(document.querySelectorAll('#funds-locked-header'), el => el.textContent)"

getFundsLockedBody :: String
getFundsLockedBody = "[].map.call(document.querySelectorAll('#funds-locked-body'), el => el.textContent)"

readString :: Foreign -> String
readString = Foreign.unsafeFromForeign
