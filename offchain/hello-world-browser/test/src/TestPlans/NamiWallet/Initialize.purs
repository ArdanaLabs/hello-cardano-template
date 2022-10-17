module HelloWorld.Test.TestPlans.NamiWallet.Initialize where

import Contract.Prelude

import Contract.Test.E2E (TestOptions, WalletExt(..))
import Data.String (Pattern(..), contains)
import HelloWorld.Test.Helpers (clickButton, getCurrentValueBody, getCurrentValueHeader, getFundsLockedBody, getFundsLockedHeader, injectJQuery, readString)
import HelloWorld.Test.NamiWallet (namiConfirmAccess', namiSign', runE2ETest)
import Mote (group)
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T

testPlan :: TestOptions -> TestPlanM Unit
testPlan testOptions = group "When initialize button is clicked" do
  runE2ETest "It shows loading dialog" testOptions NamiExt $ \example -> do
    let
      page = example.main

    injectJQuery example.jQuery page

    clickButton "Use Nami" page

    namiConfirmAccess' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: 0 } page
    clickButton "Initialize" page

    content <- T.content page
    Assert.assert "Initializing" (contains (Pattern "Initializing ...") content)

  runE2ETest "It locks ADA at contract address" testOptions NamiExt $ \example -> do
    let
      page = example.main

    injectJQuery example.jQuery page

    clickButton "Use Nami" page

    namiConfirmAccess' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: 0 } page
    clickButton "Initialize" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#current-value-header") { timeout: 0 } page
    currentValueHeaderContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueHeader page
    Assert.assert "Current value header" (contains (Pattern "Current Value") currentValueHeaderContent)

    void $ T.pageWaitForSelector (T.Selector "#current-value-body") { timeout: 0 } page
    currentValueBodyContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueBody page
    Assert.assert "Current value body" (contains (Pattern "1") currentValueBodyContent)

    void $ T.pageWaitForSelector (T.Selector "#funds-locked-header") { timeout: 0 } page
    fundsLockedHeaderContent <- readString <$> T.unsafeEvaluateStringFunction getFundsLockedHeader page
    Assert.assert "Funds locked header" (contains (Pattern "Funds Locked") fundsLockedHeaderContent)

    void $ T.pageWaitForSelector (T.Selector "#funds-locked-body") { timeout: 0 } page
    fundsLockedBodyContent <- readString <$> T.unsafeEvaluateStringFunction getFundsLockedBody page
    Assert.assert "Funds locked body" (contains (Pattern "10.0 ADA") fundsLockedBodyContent)
