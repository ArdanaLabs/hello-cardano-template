{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.String
import qualified Data.Text as T
import Debug.Trace
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.URI
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Path
import Path.IO
import System.Environment (getEnv)
import System.Exit
import Test.QuickCheck (mapSize, withMaxSuccess)
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Wai
import Test.Syd.Webdriver
import Test.WebDriver
import Test.WebDriver.Capabilities
import Test.WebDriver.Chrome.Extension (loadExtension)
import Test.WebDriver.Commands.Wait (waitUntil)
import Test.WebDriver.JSON
import UnliftIO.Path.Directory

data Command = Lock | Incr
  deriving (Show, Eq, Generic)

instance Validity Command -- Implementation is derived via Generic

instance GenValid Command

evalCommands :: [Command] -> Int
evalCommands = foldl f 0
  where
    f _ Lock = 3
    f c Incr = c + 2

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
  wdTestEnv <- webdriverTestEnvSetupFunc ssh manager uri ()
  let wdConfig = webdriverTestEnvConfig wdTestEnv
  let wdBrowser = browser $ wdCapabilities wdConfig
  case wdBrowser of
    Chrome {} -> do
      currentDir <- liftIO getCurrentDirectory
      path <- resolveFile currentDir "Nami.crx"
      mbNamiWallet <- liftIO $ forgivingAbsence $ loadExtension (fromAbsFile path)
      case mbNamiWallet of
        Nothing -> liftIO $ die "Nami wallet not loaded"
        Just namiWallet -> do
          let newBrowser = wdBrowser {chromeExtensions = [namiWallet]}
          let newWdConfig = useBrowser newBrowser wdConfig
          pure $ wdTestEnv {webdriverTestEnvConfig = newWdConfig}
    _ -> liftIO $ die "not chrome"

main :: IO ()
main = sydTest $
  setupAround (startHelloWorldBrowser >>= setupWebdriverTestEnv) $
    it "test 1" $ \wte -> mapSize (* 10) $
      withMaxSuccess 5 $
        forAllValid $ \commands ->
          runWebdriverTestM wte $ do
            openPage "chrome-extension://lpfcbjknijpeeillifnkikgncikgfhdo/mainPopup.html"

            waitUntil 5 $ findElem $ ByName "Import"

            ss <- screenshot
            liftIO $ BSL.writeFile "a.png" ss

            importBtn <- findElem $ ByName "Import"
            click importBtn

            selectInput <- findElem (ByCSS "input[type='select']")
            lastOption <- findElemsFrom selectInput $ ByCSS "input[type='option' value='24']"

            liftIO $ print lastOption

            openPath ""
            lock <- findElem $ ById "lock"
            increment <- findElem $ ById "increment"
            counter <- findElem $ ById "counter"

            let interpret c = click $ case c of
                  Lock -> lock
                  Incr -> increment

            mapM_ interpret commands
            n <- T.unpack <$> getText counter

            when (n /= show (evalCommands commands)) $ error "fail"
