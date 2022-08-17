module HelloWorld.Test.E2E.KeyWallet
  ( mkTestOptions
  , runE2ETest
  , withBrowser
  ) where

import Prelude

import Contract.Test.E2E (Mode(..), RunningExample, TestOptions(..), withExample)
import Data.Maybe (fromMaybe)
import Data.String (trim)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import HelloWorld.Test.E2E.Env as Env
import HelloWorld.Test.E2E.Helpers (helloWorldBrowserURL)
import Mote (test)
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
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
    , namiDir: ""
    , geroDir: ""
    , chromeUserDataDir
    , noHeadless: true
    }

mkTempDir :: Effect String
mkTempDir = do
  buf <- execSync "mktemp --directory" defaultExecSyncOptions
  trim <$> Buffer.toString UTF8 buf

mkChromeUserDataDir :: Effect String
mkChromeUserDataDir = do
  tempDir <- mkTempDir
  pure $ tempDir <> "/test-data/chrome-user-data"

runE2ETest
  :: forall (a :: Type)
   . String
  -> TestOptions
  -> (RunningExample -> Aff a)
  -> TestPlanM Unit
runE2ETest example opts f = test example $ withBrowser opts $
  \browser -> withExample helloWorldBrowserURL browser $ void <<< f
