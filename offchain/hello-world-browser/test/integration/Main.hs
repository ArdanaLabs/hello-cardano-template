{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.String (IsString (fromString))
import Data.Text (unpack)
import Network.HTTP.Client as HTTP (
  defaultManagerSettings,
  newManager,
 )
import Network.URI (URI, parseURI)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import System.Environment (getEnv)
import Test.Syd (
  MonadIO (liftIO),
  SetupFunc,
  describe,
  expectationFailure,
  it,
  setupAround,
  sydTest,
 )
import Test.Syd.Wai (applicationSetupFunc)
import Test.Syd.Webdriver (
  WebdriverTestEnv,
  openPath,
  runWebdriverTestM,
  seleniumServerSetupFunc,
  webdriverTestEnvSetupFunc,
 )
import Test.WebDriver (
  Selector (ById, ByTag, ByXPath),
  click,
  findElem,
  getText,
 )
import Test.WebDriver.Commands.Wait (expect, waitUntil)

startHelloWorldBrowser :: SetupFunc URI
startHelloWorldBrowser = do
  helloWorldBrowserIndex <- liftIO $ getEnv "HELLO_WORLD_BROWSER_INDEX"
  port <- applicationSetupFunc $ staticApp (defaultFileServerSettings $ fromString helloWorldBrowserIndex)
  let uriStr = "http://127.0.0.1:" <> show port
  case parseURI uriStr of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse uri as string: " <> show uriStr
    Just uri -> pure uri

setupWebdriverTestEnv :: URI -> SetupFunc (WebdriverTestEnv ())
setupWebdriverTestEnv uri = do
  ssh <- seleniumServerSetupFunc
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  webdriverTestEnvSetupFunc ssh manager uri ()

main :: IO ()
main = sydTest $
  setupAround (startHelloWorldBrowser >>= setupWebdriverTestEnv) $ do
    describe "When initialize button is clicked" $ do
      it "locks ADA at contract address" $ \wte ->
        runWebdriverTestM wte $ do
          openPath ""
          waitUntil 10 $ findElem $ ByTag "main"
          main <- findElem $ ByTag "main"

          waitUntil 10 $ findElem $ ById "lock"
          lockBtn <- findElem $ ById "lock"
          click lockBtn

          waitUntil 10 $ findElem $ ById "current-value-header"
          currentValueHeader <- findElem $ ById "current-value-header"
          waitUntil 10 $ expect . (== "Current Value") =<< getText currentValueHeader

          waitUntil 10 $ findElem $ ById "funds-locked-header"
          fundsLockedHeader <- findElem $ ById "funds-locked-header"
          waitUntil 10 $ expect . (== "Funds Locked") =<< getText fundsLockedHeader

          waitUntil 10 $ findElem $ ById "current-value-body"
          currentValueBody <- findElem $ ById "current-value-body"
          waitUntil 10 $ expect . (== "3") =<< getText currentValueBody

          waitUntil 10 $ findElem $ ById "funds-locked-body"
          fundsLockedBody <- findElem $ ById "funds-locked-body"
          waitUntil 10 $ expect . (== "6.0 ADA") =<< getText fundsLockedBody

          pure ()

      it "shows loading dialogue" $ \wte ->
        runWebdriverTestM wte $ do
          openPath ""
          waitUntil 10 $ findElem $ ByTag "main"
          main <- findElem $ ByTag "main"

          waitUntil 10 $ findElem $ ById "lock"
          lockBtn <- findElem $ ById "lock"
          click lockBtn

          waitUntil 10 $ findElem $ ByXPath "//*[text()='Initializing']"

          pure ()

    describe "When increment button is clicked" $ do
      it "increments the datum at script address by 2" $ \wte ->
        runWebdriverTestM wte $ do
          openPath ""
          waitUntil 10 $ findElem $ ByTag "main"
          main <- findElem $ ByTag "main"

          waitUntil 10 $ findElem $ ById "lock"
          lockBtn <- findElem $ ById "lock"
          click lockBtn

          waitUntil 10 $ findElem $ ById "increment"
          incrementBtn <- findElem $ ById "increment"
          click incrementBtn

          waitUntil 10 $ findElem $ ById "current-value-body"
          currentValueBody <- findElem $ ById "current-value-body"
          waitUntil 10 $ expect . (== "5") =<< getText currentValueBody

          pure ()

      it "shows loading dialogue" $ \wte ->
        runWebdriverTestM wte $ do
          openPath ""
          waitUntil 10 $ findElem $ ByTag "main"
          main <- findElem $ ByTag "main"

          waitUntil 10 $ findElem $ ById "lock"
          lockBtn <- findElem $ ById "lock"
          click lockBtn

          waitUntil 10 $ findElem $ ById "increment"
          incrementBtn <- findElem $ ById "increment"
          click incrementBtn

          waitUntil 10 $ findElem $ ByXPath "//*[text()='Incrementing from 3 to 5 ...']"

          pure ()

    describe "When redeem button is clicked" $ do
      it "redeems the funds locked at script address by 2" $ \wte ->
        runWebdriverTestM wte $ do
          openPath ""
          waitUntil 10 $ findElem $ ByTag "main"
          main <- findElem $ ByTag "main"

          waitUntil 10 $ findElem $ ById "lock"
          lockBtn <- findElem $ ById "lock"
          click lockBtn

          waitUntil 10 $ findElem $ ById "redeem"
          redeemBtn <- findElem $ ById "redeem"
          click redeemBtn

          waitUntil 10 $ findElem $ ById "lock"

          pure ()

      it "shows loading dialogue" $ \wte ->
        runWebdriverTestM wte $ do
          openPath ""
          waitUntil 10 $ findElem $ ByTag "main"
          main <- findElem $ ByTag "main"

          waitUntil 10 $ findElem $ ById "lock"
          lockBtn <- findElem $ ById "lock"
          click lockBtn

          waitUntil 10 $ findElem $ ById "redeem"
          redeemBtn <- findElem $ ById "redeem"
          click redeemBtn

          waitUntil 10 $ findElem $ ByXPath "//*[text()='Redeeming 6.0 ADA ...']"

          pure ()
