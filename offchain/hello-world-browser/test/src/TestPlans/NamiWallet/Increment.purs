module HelloWorld.Test.TestPlans.NamiWallet.Increment where

import Contract.Prelude

import Contract.Test.E2E (TestOptions, WalletExt(..), delaySec)
import Data.String (Pattern(..), contains)
import HelloWorld.Test.Constants as Constants
import HelloWorld.Test.Helpers (clickButton, getCurrentValueBody, injectJQuery, readString)
import HelloWorld.Test.NamiWallet (namiConfirmAccess', namiSign', runE2ETest)
import Mote (group)
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T

testPlan :: TestOptions -> TestPlanM Unit
testPlan testOptions = group "When increment button is clicked" do
  runE2ETest "It shows loading dialog" testOptions NamiExt $ \example -> do
    let
      page = example.main

    injectJQuery example.jQuery page

    clickButton "Initialize" page

    namiConfirmAccess' example

    delaySec Constants.threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#increment") { timeout: Constants.timeoutMs } page
    clickButton "+" page

    content <- T.content page
    Assert.assert "Incrementing" (contains (Pattern "Incrementing from 3 to 5 ...") content)

  runE2ETest "It increments the datum at script address by 2" testOptions NamiExt $ \example -> do
    let
      page = example.main

    injectJQuery example.jQuery page

    clickButton "Initialize" page

    namiConfirmAccess' example

    delaySec Constants.threeSeconds

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#increment") { timeout: Constants.timeoutMs } page
    clickButton "+" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#current-value-body") { timeout: Constants.timeoutMs } page
    currentValueBodyContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueBody page
    Assert.assert "Current value body" (contains (Pattern "5") currentValueBodyContent)
