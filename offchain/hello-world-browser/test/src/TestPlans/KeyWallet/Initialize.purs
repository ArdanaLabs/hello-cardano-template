module HelloWorld.Test.TestPlans.KeyWallet.Initialize where

import Contract.Prelude

import Contract.Test.E2E (TestOptions)
import Data.String (Pattern(..), contains)
import HelloWorld.Test.Constants as Constants
import HelloWorld.Test.Helpers (clickButton, getCurrentValueBody, getCurrentValueHeader, getFundsLockedBody, getFundsLockedHeader, injectJQuery, readString)
import HelloWorld.Test.KeyWallet (RunningExample(..), runE2ETest)
import Mote (group)
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T

testPlan :: TestOptions -> TestPlanM Unit
testPlan testOptions = group "When initialize button is clicked" do
  runE2ETest "It shows loading dialog" testOptions $ \(RunningExample { jQuery, page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page
    content <- T.content page
    Assert.assert "Initializing" (contains (Pattern "Initializing ...") content)

  runE2ETest "It locks ADA at contract address" testOptions $ \(RunningExample { jQuery, page }) -> do
    injectJQuery jQuery page

    clickButton "Initialize" page

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
