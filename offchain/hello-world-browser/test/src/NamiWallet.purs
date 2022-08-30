module HelloWorld.Test.NamiWallet where

import Contract.Prelude

import Contract.Test.E2E (Mode(..), RunningExample, TestOptions(..), WalletExt(..), delaySec, namiSign, withExample)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (trim)
import Effect (Effect)
import Effect.Aff (bracket, try)
import Effect.Exception (throw)
import HelloWorld.Test.Constants (PaymentAddress)
import HelloWorld.Test.Constants as Constants
import HelloWorld.Test.Env as Env
import HelloWorld.Test.Helpers (helloWorldBrowserURL)
import Mote (test)
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Node.Process (lookupEnv)
import Plutip.Types (FilePath)
import TestM (TestPlanM)
import Toppokki as T

data NamiWallet = NamiWallet
  { paymentAddress :: PaymentAddress
  , settingsFile :: String
  }

mkNamiWallet :: PaymentAddress -> String -> Effect NamiWallet
mkNamiWallet paymentAddress env = do
  lookupEnv env >>= case _ of
    Nothing -> throw $ env <> " not found"
    Just settingsFile -> pure $ NamiWallet
      { paymentAddress
      , settingsFile
      }

testWallet1 :: Effect NamiWallet
testWallet1 = mkNamiWallet Constants.paymentAddressForTestWallet1 Env.namiWallet1

testWallet2 :: Effect NamiWallet
testWallet2 = mkNamiWallet Constants.paymentAddressForTestWallet2 Env.namiWallet2

testWallet3 :: Effect NamiWallet
testWallet3 = mkNamiWallet Constants.paymentAddressForTestWallet3 Env.namiWallet3

mkTempDir :: Effect String
mkTempDir = do
  buf <- execSync "mktemp --directory" defaultExecSyncOptions
  trim <$> Buffer.toString UTF8 buf

unzipNamiSettings :: String -> String -> Effect Unit
unzipNamiSettings tmpDir settingsFile =
  void $ execSync ("tar zxf " <> settingsFile <> " --directory " <> tmpDir) defaultExecSyncOptions

unzipNamiExtension :: String -> Effect Unit
unzipNamiExtension tmpDir = do
  namiExtension <- lookupEnv Env.namiExtension
  case namiExtension of
    Nothing -> throw "NAMI_EXTENSION not set"
    Just extension ->
      void $ execSync ("unzip " <> extension <> " -d " <> tmpDir <> "/nami > /dev/zero || echo \"ignore warnings\"") defaultExecSyncOptions

topup :: NamiWallet -> Effect Unit
topup (NamiWallet { paymentAddress }) = do
  let url = Constants.faucetUrl <> (unwrap paymentAddress) <> "?apiKey=" <> Constants.faucetApiKey
  void $ execSync ("curl -XPOST " <> url) defaultExecSyncOptions

mkTestOptions :: NamiWallet -> Effect TestOptions
mkTestOptions testWallet = do
  chromeExe <- lookupEnv Env.chromeExe
  NamiDir namiDir <- mkNamiDir
  ChromeUserDataDir chromeUserDataDir <- mkChromeUserDataDir testWallet

  pure $ TestOptions
    { chromeExe
    , namiDir
    , geroDir: ""
    , chromeUserDataDir
    , noHeadless: false
    }

newtype NamiDir = NamiDir String

newtype ChromeUserDataDir = ChromeUserDataDir String

mkNamiDir :: Effect NamiDir
mkNamiDir = do
  tempDir <- mkTempDir
  unzipNamiExtension tempDir
  pure (NamiDir $ tempDir <> "/nami")

mkChromeUserDataDir :: NamiWallet -> Effect ChromeUserDataDir
mkChromeUserDataDir (NamiWallet { settingsFile }) = do
  tempDir <- mkTempDir
  unzipNamiSettings tempDir settingsFile
  pure (ChromeUserDataDir $ tempDir <> "/test-data/chrome-user-data")

runE2ETest
  :: forall (a :: Type)
   . String
  -> TestOptions
  -> WalletExt
  -> (RunningExample -> Aff a)
  -> TestPlanM Unit
runE2ETest example opts ext f = test example $ withBrowser opts ext $
  \browser -> withExample helloWorldBrowserURL browser $ void <<< f

launchWithExtension :: WalletExt -> TestOptions -> Aff T.Browser
launchWithExtension walletExt testOptions@(TestOptions { chromeExe, chromeUserDataDir, namiDir, geroDir, noHeadless }) = do
  result <- try $ T.launch
    { args:
        [ "--disable-extensions-except=" <> extDir
        , "--load-extension=" <> extDir
        ] <> if mode == Headless then [ "--headless=chrome" ] else []
    , headless: mode == Headless
    , userDataDir: chromeUserDataDir
    , executablePath: fromMaybe "" chromeExe
    }
  case result of
    Left _ -> do
      delaySec Constants.tenSeconds
      launchWithExtension walletExt testOptions
    Right browser -> pure browser
  where
  mode :: Mode
  mode
    | noHeadless = Visible
    | otherwise = Headless

  extDir :: FilePath
  extDir = case walletExt of
    GeroExt -> geroDir
    NamiExt -> namiDir

withBrowser
  :: forall (a :: Type)
   . TestOptions
  -> WalletExt
  -> (T.Browser -> Aff a)
  -> Aff a
withBrowser opts ext = bracket (launchWithExtension ext opts) T.close

namiSign' :: RunningExample -> Aff Unit
namiSign' = namiSign Constants.namiWalletPassword

