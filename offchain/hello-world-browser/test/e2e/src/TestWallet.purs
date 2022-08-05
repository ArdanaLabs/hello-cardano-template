module HelloWorld.Test.E2E.TestWallet where

import Prelude

import Contract.Test.E2E (TestOptions(..))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Effect (Effect)
import Effect.Exception (throw)
import HelloWorld.Test.E2E.Env as Env
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Node.Process (lookupEnv)

data TestWallet = TestWallet
  { paymentAddress :: String
  , settingsFile :: String
  }

mkTestWallet :: String -> String -> Effect TestWallet
mkTestWallet paymentAddress env = do
  lookupEnv env >>= case _ of
    Nothing -> throw $ env <> " not found"
    Just settingsFile -> pure $ TestWallet
      { paymentAddress
      , settingsFile
      }

paymentAddressForTestWallet1 :: String
paymentAddressForTestWallet1 = "addr_test1qrgzwhfw2w63m7up7swqpcg5r52c05sgwatn4697kgle009akzxrh366l0rqsvjm3q9pyd9fm6t4cegfm286r5lvwh6sa94d5d"

paymentAddressForTestWallet2 :: String
paymentAddressForTestWallet2 = "addr_test1qp7h5urcrtq67nl2tcz473lzfw29kpl7kn5xxmyxm6yjxk0u6prmwvzm4s48m6v5u4fyfcddaxz27g96880rh03248zsy38gnr"

paymentAddressForTestWallet3 :: String
paymentAddressForTestWallet3 = "addr_test1qpe8yzu8g9pdqq6xxyv2gm3rn39fwcp44pnx6mmng7nqhz3apa7n8svjgrag8vjn9v5juf2wgzfzh0wzxyezlachfx2san47c2"

testWallet1 :: Effect TestWallet
testWallet1 = mkTestWallet paymentAddressForTestWallet1 Env.namiWallet1

testWallet2 :: Effect TestWallet
testWallet2 = mkTestWallet paymentAddressForTestWallet2 Env.namiWallet2

testWallet3 :: Effect TestWallet
testWallet3 = mkTestWallet paymentAddressForTestWallet3 Env.namiWallet3

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

faucetApiKey :: String
faucetApiKey = "r8m9YXmqCkFWDDZ2540IJaJwr1JBxqXB"

topup :: String -> Effect Unit
topup paymentAddress = do
  let url = "https://faucet.cardano-testnet.iohkdev.io/send-money/" <> paymentAddress <> "?apiKey=" <> faucetApiKey
  void $ execSync ("curl -XPOST " <> url) defaultExecSyncOptions

mkTestOptions :: TestWallet -> Effect TestOptions
mkTestOptions (TestWallet { paymentAddress, settingsFile }) = do
  chromeExe <- lookupEnv Env.chromeExe

  testData <- mkTempDir
  unzipNamiExtension testData
  unzipNamiSettings testData settingsFile

  case mkTestOptions' <$> Just testData <*> chromeExe of
    Nothing -> throw "failed to setup test options"
    Just testOptions -> do
      topup paymentAddress
      pure testOptions
  where
  mkTestOptions' :: String -> String -> TestOptions
  mkTestOptions' testData chromeExe =
    TestOptions
      { chromeExe: Just chromeExe
      , namiDir: testData <> "/nami"
      , geroDir: testData <> "/gero"
      , chromeUserDataDir: testData <> "/test-data/chrome-user-data"
      , noHeadless: false
      }