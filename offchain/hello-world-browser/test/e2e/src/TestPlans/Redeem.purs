module HelloWorld.Test.E2E.TestPlans.Redeem where

import Contract.Prelude

import Contract.Test.E2E (RunningExample(..), TestOptions, WalletExt(..), delaySec, namiConfirmAccess)
import Data.String (Pattern(..), contains)
import HelloWorld.Test.E2E.Constants as Constants
import HelloWorld.Test.E2E.Helpers (clickButton, injectJQuery, namiSign', runE2ETest)
import Mote (group)
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T

testPlan :: TestOptions -> TestPlanM Unit
testPlan testOptions = group "When redeem button is clicked" do
  runE2ETest "It shows loading dialogue" testOptions NamiExt $ \example@(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    namiConfirmAccess example

    delaySec Constants.threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: Constants.timeoutMs } page
    clickButton "Redeem" page

    content <- T.content page
    Assert.assert "Redeeming" (contains (Pattern "Redeeming 10.0 ADA ...") content)

  runE2ETest "It goes to initial page" testOptions NamiExt $ \example@(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    namiConfirmAccess example

    delaySec Constants.threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: Constants.timeoutMs } page
    clickButton "Redeem" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: Constants.timeoutMs } page