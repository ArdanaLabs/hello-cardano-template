{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Maybe (isJust)
import Network.HTTP.Client as HTTP (
  defaultManagerSettings,
  newManager,
 )
import Network.URI (URI, parseURI)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
import Test.Syd (
  MonadIO (liftIO),
  SetupFunc,
  Spec,
  describe,
  it,
  setupAround,
  sydTest,
 )
import Test.Syd.Path (tempDirSetupFunc)
import Test.Syd.Wai (applicationSetupFunc, waiSpec)
import Test.Syd.Webdriver (
  WebdriverTestEnv (webdriverTestEnvConfig),
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

setupWebdriverTestEnv :: SetupFunc (WebdriverTestEnv ())
setupWebdriverTestEnv = do
  helloWorldBrowserIndex <- liftIO $ getEnv "HELLO_WORLD_BROWSER_INDEX"
  port <- applicationSetupFunc $ staticApp (defaultFileServerSettings helloWorldBrowserIndex)
  let uriStr = "http://127.0.0.1:" <> show port
  uri <- case parseURI uriStr of
    Nothing -> liftIO $ die ("Failed to parse uri as string: " <> show uriStr)
    Just uri -> pure uri
  ssh <- seleniumServerSetupFunc
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  webdriverTestEnvSetupFunc ssh manager uri ()

timeoutSeconds :: Double
timeoutSeconds = 240

main :: IO ()
main = do
  noRuntime <- isJust <$> lookupEnv "NO_RUNTIME"
  if noRuntime
    then print "skip the test since there's no ctl-runtime"
    else sydTest $
      setupAround setupWebdriverTestEnv $ do
        it "happy path" $ \wte ->
          runWebdriverTestM wte $ do
            openPath ""
            waitUntil timeoutSeconds $ findElem $ ByTag "main"
            main <- findElem $ ByTag "main"

            waitUntil timeoutSeconds $ findElem $ ById "lock"
            lockBtn <- findElem $ ById "lock"
            click lockBtn

            waitUntil timeoutSeconds $ findElem $ ByXPath "//*[text()='Initializing ...']"

            waitUntil timeoutSeconds $ findElem $ ById "current-value-header"
            currentValueHeader <- findElem $ ById "current-value-header"
            waitUntil timeoutSeconds $ expect . (== "Current Value") =<< getText currentValueHeader

            waitUntil timeoutSeconds $ findElem $ ById "funds-locked-header"
            fundsLockedHeader <- findElem $ ById "funds-locked-header"
            waitUntil timeoutSeconds $ expect . (== "Funds Locked") =<< getText fundsLockedHeader

            waitUntil timeoutSeconds $ findElem $ ById "current-value-body"
            currentValueBody <- findElem $ ById "current-value-body"
            waitUntil timeoutSeconds $ expect . (== "3") =<< getText currentValueBody

            waitUntil timeoutSeconds $ findElem $ ById "funds-locked-body"
            fundsLockedBody <- findElem $ ById "funds-locked-body"
            waitUntil timeoutSeconds $ expect . (== "10.0 ADA") =<< getText fundsLockedBody

            waitUntil timeoutSeconds $ findElem $ ById "increment"
            incrementBtn <- findElem $ ById "increment"
            click incrementBtn

            waitUntil timeoutSeconds $ findElem $ ByXPath "//*[text()='Incrementing from 3 to 5 ...']"

            waitUntil timeoutSeconds $ findElem $ ById "current-value-body"
            currentValueBody <- findElem $ ById "current-value-body"
            waitUntil timeoutSeconds $ expect . (== "5") =<< getText currentValueBody

            waitUntil timeoutSeconds $ findElem $ ById "redeem"
            redeemBtn <- findElem $ ById "redeem"
            click redeemBtn

            waitUntil timeoutSeconds $ findElem $ ByXPath "//*[text()='Redeeming 10.0 ADA ...']"

            waitUntil timeoutSeconds $ findElem $ ById "lock"

            pure ()
