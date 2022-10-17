module HelloWorld.Test.TestPlans.NamiWallet.Increment where

import Contract.Prelude

import Contract.Test.E2E (TestOptions, WalletExt(..))
import Data.String (Pattern(..), contains)
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

    clickButton "Use Nami" page

    namiConfirmAccess' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: 0 } page
    clickButton "Initialize" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#increment") { timeout: 0 } page
    clickButton "+" page

    content <- T.content page
    Assert.assert "Incrementing" (contains (Pattern "Incrementing from 1 to 3 ...") content)

  runE2ETest "It increments the datum at script address by 2" testOptions NamiExt $ \example -> do
    let
      page = example.main

    injectJQuery example.jQuery page

    clickButton "Use Nami" page

    namiConfirmAccess' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: 0 } page
    clickButton "Initialize" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#increment") { timeout: 0 } page
    clickButton "+" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#current-value-body") { timeout: 0 } page
    currentValueBodyContent <- readString <$> T.unsafeEvaluateStringFunction getCurrentValueBody page
    Assert.assert "Current value body" (contains (Pattern "3") currentValueBodyContent)
