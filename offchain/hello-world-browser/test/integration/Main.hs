{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad (unless, when)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Network.HTTP.Client as HTTP (
  defaultManagerSettings,
  newManager,
 )
import Network.URI (URI, parseURI)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Path (fromAbsFile)
import Path.IO (forgivingAbsence, getCurrentDir, resolveFile)
import System.Environment (getEnv)
import System.Exit (die)
import Test.QuickCheck (mapSize, withMaxSuccess)
import Test.Syd (
  MonadIO (liftIO),
  SetupFunc,
  expectationFailure,
  it,
  setupAround,
  sydTest,
 )
import Test.Syd.Validity (GenValid, Validity, forAllValid)
import Test.Syd.Wai (applicationSetupFunc, methodDelete, methodGet, methodPost)
import Test.Syd.Webdriver (
  WebdriverTestEnv (webdriverTestEnvConfig),
  openPath,
  runWebdriverTestM,
  seleniumServerSetupFunc,
  webdriverTestEnvSetupFunc,
 )
import Test.WebDriver (
  Browser (Chrome, chromeExtensions, chromeOptions),
  Capabilities (browser),
  Selector (ByClass, ById, ByXPath),
  WDConfig (wdCapabilities),
  click,
  closeWindow,
  findElem,
  getCurrentWindow,
  getText,
  openPage,
  sendKeys,
  useBrowser,
  windows,
 )
import Test.WebDriver.Chrome.Extension (loadExtension)
import Test.WebDriver.Class (WebDriver (..))
import Test.WebDriver.Commands.Wait (expect, waitUntil)
import UnliftIO.Path.Directory (getCurrentDirectory)

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
          let newBrowser =
                wdBrowser
                  { chromeExtensions = [namiWallet]
                  , chromeOptions =
                      [ --"--headless=chrome",
                        "--no-sandbox" -- Bypass OS security model to run on nix as well
                      , "--disable-dev-shm-usage" -- Overcome limited resource problem
                      , "--disable-gpu"
                      , "--use-gl=angle"
                      , "--use-angle=swiftshader"
                      , "--window-size=1920,1080"
                      ]
                  }
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

            importBtn <- findElem $ ByXPath "//button[text()='Import']"
            click importBtn

            selectInput <- findElem $ ByXPath "//select"
            sendKeys "24" selectInput

            agreeBtn <- findElem $ ByClass "chakra-checkbox"
            click agreeBtn

            continueBtn <- findElem $ ByXPath "//button[text()='Continue']"
            click continueBtn

            getCurrentWindow >>= closeWindow

            word1Input <- findElem $ ByXPath "//input[@placeholder='Word 1']"
            sendKeys "abc" word1Input

            word2Input <- findElem $ ByXPath "//input[@placeholder='Word 2']"
            sendKeys "def" word2Input

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
