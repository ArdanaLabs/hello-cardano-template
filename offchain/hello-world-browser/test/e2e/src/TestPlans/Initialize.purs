module HelloWorld.Test.E2E.TestPlans.Initialize where

import Contract.Prelude

import Contract.Test.E2E (RunningExample(..), TestOptions, WalletExt(..), delaySec, namiConfirmAccess)
import Data.String (Pattern(..), contains)
import HelloWorld.Test.E2E.Constants as Constants
import HelloWorld.Test.E2E.Helpers (clickButton, getCurrentValueBody, getCurrentValueHeader, getFundsLockedBody, getFundsLockedHeader, injectJQuery, namiSign', readString, runE2ETest)
import Mote (group)
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T

testPlan :: TestOptions -> TestPlanM Unit
testPlan testOptions = group "When initialize button is clicked" do
  runE2ETest "It shows loading dialog" testOptions NamiExt $ \(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page
    content <- T.content page
    Assert.assert "Initializing" (contains (Pattern "Initializing ...") content)

  runE2ETest "It locks ADA at contract address" testOptions NamiExt $ \example@(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    namiConfirmAccess example

    delaySec Constants.threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#current-value-header") { timeout: Constants.timeoutMs } page
    currentValueHeaderContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueHeader page
    Assert.assert "Current value header" (contains (Pattern "Current Value") currentValueHeaderContent)

    void $ T.pageWaitForSelector (T.Selector "#current-value-body") { timeout: Constants.timeoutMs } page
    currentValueBodyContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueBody page
    Assert.assert "Current value body" (contains (Pattern "3") currentValueBodyContent)

    void $ T.pageWaitForSelector (T.Selector "#funds-locked-header") { timeout: Constants.timeoutMs } page
    fundsLockedHeaderContent <- readString <$> T.unsafeEvaluateStringFunction getFundsLockedHeader page
    Assert.assert "Funds locked header" (contains (Pattern "Funds Locked") fundsLockedHeaderContent)

    void $ T.pageWaitForSelector (T.Selector "#funds-locked-body") { timeout: Constants.timeoutMs } page
    fundsLockedBodyContent <- readString <$> T.unsafeEvaluateStringFunction getFundsLockedBody page
    Assert.assert "Funds locked body" (contains (Pattern "10.0 ADA") fundsLockedBodyContent)