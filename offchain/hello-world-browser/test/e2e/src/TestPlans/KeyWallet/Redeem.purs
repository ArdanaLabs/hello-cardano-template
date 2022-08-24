module HelloWorld.Test.E2E.TestPlans.KeyWallet.Redeem where

import Contract.Prelude

import Contract.Test.E2E (RunningExample(..), TestOptions)
import Data.String (Pattern(..), contains)
import HelloWorld.Test.E2E.Constants as Constants
import HelloWorld.Test.E2E.Helpers (clickButton, injectJQuery)
import HelloWorld.Test.E2E.KeyWallet (runE2ETest)
import Mote (group)
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T

testPlan :: TestOptions -> TestPlanM Unit
testPlan testOptions = group "When redeem button is clicked" do
  runE2ETest "It shows loading dialog" testOptions $ \(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: Constants.timeoutMs } page
    clickButton "Redeem" page

    content <- T.content page
    Assert.assert "Redeeming" (contains (Pattern "Redeeming 10.0 ADA ...") content)

  runE2ETest "It goes to initial page" testOptions $ \(RunningExample { jQuery, main: page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: Constants.timeoutMs } page
    clickButton "Redeem" page

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: Constants.timeoutMs } page