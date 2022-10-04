module HelloWorld.Test.KeyWallet
  ( mkTestOptions
  , runE2ETest
  , withBrowser
  , RunningExample(..)
  ) where

import Contract.Prelude

import Contract.Test.E2E (Mode(..), TestOptions(..))
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Exception (throw)
import HelloWorld.Test.Env as Env
import HelloWorld.Test.Helpers (helloWorldBrowserURL)
import Mote (test)
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (lookupEnv)
import TestM (TestPlanM)
import Toppokki as Toppokki

withBrowser
  :: forall (a :: Type)
   . TestOptions
  -> (Toppokki.Browser -> Aff a)
  -> Aff a
withBrowser opts = bracket (launch opts) Toppokki.close

launch :: TestOptions -> Aff Toppokki.Browser
launch (TestOptions { chromeExe, chromeUserDataDir, noHeadless }) = do
  Toppokki.launch
    { args: if mode == Headless then [ "--headless=chrome" ] else []
    , headless: mode == Headless
    , userDataDir: chromeUserDataDir
    , executablePath: fromMaybe "" chromeExe
    }
  where
  mode :: Mode
  mode
    | noHeadless = Visible
    | otherwise = Headless

mkTestOptions :: Effect TestOptions
mkTestOptions = do
  chromeExe <- lookupEnv Env.chromeExe
  chromeUserDataDir <- mkChromeUserDataDir

  pure $ TestOptions
    { chromeExe
    , wallets: Map.empty
    , chromeUserDataDir
    , noHeadless: false
    }

mkTempDir :: Effect String
mkTempDir = do
  buf <- execSync "mktemp --directory" defaultExecSyncOptions
  trim <$> Buffer.toString UTF8 buf

mkChromeUserDataDir :: Effect String
mkChromeUserDataDir = do
  tempDir <- mkTempDir
  pure $ tempDir <> "/test-data/chrome-user-data"

newtype RunningExample = RunningExample
  { browser :: Toppokki.Browser
  , jQuery :: String
  , page :: Toppokki.Page
  }

derive instance Newtype RunningExample _

startExample :: Toppokki.URL -> Toppokki.Browser -> Aff RunningExample
startExample url browser = do
  page <- Toppokki.newPage browser

  jQuery <- liftEffect $ lookupEnv "JQUERY_MIN_SRC" >>= \mSrc -> do
    case mSrc of
      Nothing -> throw "Failed to load jQuery"
      Just src -> readTextFile UTF8 src
  Toppokki.goto url page
  pure $ wrap
    { browser: browser
    , jQuery: jQuery
    , page: page
    }

withExample
  :: forall (a :: Type)
   . Toppokki.URL
  -> Toppokki.Browser
  -> (RunningExample -> Aff a)
  -> Aff a
withExample url browser = bracket (startExample url browser)
  (const $ pure unit)

runE2ETest
  :: forall (a :: Type)
   . String
  -> TestOptions
  -> (RunningExample -> Aff a)
  -> TestPlanM Unit
runE2ETest example opts f = test example $ withBrowser opts $
  \browser -> withExample helloWorldBrowserURL browser $ void <<< f
