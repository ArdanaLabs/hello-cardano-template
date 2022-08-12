module HelloWorld.Test.E2E.TestWallet where

import Prelude

import Contract.Test.E2E (TestOptions(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (trim)
import Effect (Effect)
import Effect.Exception (throw)
import HelloWorld.Test.E2E.Constants (PaymentAddress)
import HelloWorld.Test.E2E.Constants as Constants
import HelloWorld.Test.E2E.Env as Env
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Node.Process (lookupEnv)

data TestWallet = TestWallet
  { paymentAddress :: PaymentAddress
  , settingsFile :: String
  }

mkTestWallet :: PaymentAddress -> String -> Effect TestWallet
mkTestWallet paymentAddress env = do
  lookupEnv env >>= case _ of
    Nothing -> throw $ env <> " not found"
    Just settingsFile -> pure $ TestWallet
      { paymentAddress
      , settingsFile
      }

testWallet1 :: Effect TestWallet
testWallet1 = mkTestWallet Constants.paymentAddressForTestWallet1 Env.namiWallet1

testWallet2 :: Effect TestWallet
testWallet2 = mkTestWallet Constants.paymentAddressForTestWallet2 Env.namiWallet2

testWallet3 :: Effect TestWallet
testWallet3 = mkTestWallet Constants.paymentAddressForTestWallet3 Env.namiWallet3

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

topup :: TestWallet -> Effect Unit
topup (TestWallet { paymentAddress }) = do
  let url = "https://faucet.cardano-testnet.iohkdev.io/send-money/" <> (unwrap paymentAddress) <> "?apiKey=" <> Constants.faucetApiKey
  void $ execSync ("curl -XPOST " <> url) defaultExecSyncOptions

mkTestOptions :: TestWallet -> Effect TestOptions
mkTestOptions testWallet = do
  chromeExe <- lookupEnv Env.chromeExe
  NamiDir namiDir <- mkNamiDir
  ChromeUserDataDir chromeUserDataDir <- mkChromeUserDataDir testWallet

  pure $ TestOptions
    { chromeExe
    , namiDir
    , geroDir: ""
    , chromeUserDataDir
    , noHeadless: true
    }

newtype NamiDir = NamiDir String

newtype ChromeUserDataDir = ChromeUserDataDir String

mkNamiDir :: Effect NamiDir
mkNamiDir = do
  tempDir <- mkTempDir
  unzipNamiExtension tempDir
  pure (NamiDir $ tempDir <> "/nami")

mkChromeUserDataDir :: TestWallet -> Effect ChromeUserDataDir
mkChromeUserDataDir (TestWallet { settingsFile }) = do
  tempDir <- mkTempDir
  unzipNamiSettings tempDir settingsFile
  pure (ChromeUserDataDir $ tempDir <> "/test-data/chrome-user-data")