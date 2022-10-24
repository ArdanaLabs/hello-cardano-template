module HelloWorld.Test.TestPlans.NamiWallet.Redeem where

import Contract.Prelude

import Contract.Test.E2E (TestOptions, WalletExt(..))
import Data.String (Pattern(..), contains)
import HelloWorld.Test.Helpers (clickButton, injectJQuery)
import HelloWorld.Test.NamiWallet (namiConfirmAccess', namiSign', runE2ETest)
import Mote (group)
import Test.Unit.Assert as Assert
import TestM (TestPlanM)
import Toppokki as T

testPlan :: TestOptions -> TestPlanM Unit
testPlan testOptions = group "When redeem button is clicked" do
  runE2ETest "It shows loading dialogue" testOptions NamiExt $ \example -> do
    let
      page = example.main

    injectJQuery example.jQuery page

    clickButton "Use Nami" page

    namiConfirmAccess' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: 0 } page
    clickButton "Initialize" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: 0 } page
    clickButton "Redeem" page

    content <- T.content page
    Assert.assert "Redeeming" (contains (Pattern "Redeeming 10.0 ADA ...") content)

  runE2ETest "It shows unlocking funds dialog" testOptions NamiExt $ \example -> do
    let
      page = example.main

    injectJQuery example.jQuery page

    clickButton "Use Nami" page

    namiConfirmAccess' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: 0 } page
    clickButton "Initialize" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: 0 } page
    clickButton "Redeem" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#unlocking") { timeout: 0 } page

    content <- T.content page
    Assert.assert "Unlocking funds" (contains (Pattern "Unlocking funds ...") content)

  runE2ETest "It goes to initial page" testOptions NamiExt $ \example -> do
    let
      page = example.main

    injectJQuery example.jQuery page

    clickButton "Use Nami" page

    namiConfirmAccess' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: 0 } page
    clickButton "Initialize" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#redeem") { timeout: 0 } page
    clickButton "Redeem" page

    namiSign' example

    void $ T.pageWaitForSelector (T.Selector "#lock") { timeout: 0 } page
