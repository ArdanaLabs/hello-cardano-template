module HelloWorld.Test.TestPlans.KeyWallet.Redeem where

import Contract.Prelude

import Contract.Test.E2E (TestOptions)
import Data.String (Pattern(..), contains)
import HelloWorld.Test.Constants as Constants
import HelloWorld.Test.Helpers (clickButton, injectJQuery)
import HelloWorld.Test.KeyWallet (RunningExample(..), runE2ETest)
import Mote (group)
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T

testPlan :: TestOptions -> TestPlanM Unit
testPlan testOptions = group "When redeem button is clicked" do
  runE2ETest "It shows loading dialog" testOptions $ \(RunningExample { jQuery, page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: Constants.timeoutMs } page
    clickButton "Redeem" page

    content <- T.content page
    Assert.assert "Redeeming" (contains (Pattern "Redeeming 10.0 ADA ...") content)

  runE2ETest "It shows unlocking funds dialog" testOptions $ \(RunningExample { jQuery, page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: Constants.timeoutMs } page
    clickButton "Redeem" page

    void $ T.pageWaitForSelector (T.Selector "#unlocking") { timeout: Constants.timeoutMs } page

    content <- T.content page
    Assert.assert "Unlocking funds" (contains (Pattern "Unlocking funds ...") content)

  runE2ETest "It goes to initial page" testOptions $ \(RunningExample { jQuery, page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: Constants.timeoutMs } page
    clickButton "Redeem" page

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: Constants.timeoutMs } page
