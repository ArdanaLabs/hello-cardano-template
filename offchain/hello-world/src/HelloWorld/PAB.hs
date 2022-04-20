{-# LANGUAGE DeriveAnyClass #-}

module HelloWorld.PAB (
  HelloWorldContracts (..),
  runPAB,
) where

import Cardano.Api qualified as CAPI
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (NetworkIdWrapper))
import Cardano.ChainIndex.Types (ChainIndexConfig (ciBaseUrl), ChainIndexUrl (ChainIndexUrl))
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Launcher.Node qualified as CNode
import Cardano.Mnemonic (SomeMnemonic (..))
import Cardano.Node.Types (NodeMode (AlonzoNode), PABServerConfig (..))
import Cardano.Wallet.Api.Client qualified as WalletClient
import Cardano.Wallet.Api.Types (
  ApiMnemonicT (..),
  ApiT (..),
  ApiWallet (ApiWallet),
  WalletOrAccountPostData (..),
 )
import Cardano.Wallet.Api.Types qualified as Wallet.Types
import Cardano.Wallet.Primitive.AddressDerivation (Passphrase (..))
import Cardano.Wallet.Primitive.Types (WalletName (..))
import Cardano.Wallet.Shelley.Launch (CardanoNodeConn)
import Cardano.Wallet.Types qualified as Wallet
import Control.Concurrent qualified as CC
import Control.Concurrent.Async (async)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (void)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Asn
import Data.Default (Default (def))
import Data.OpenApi.Schema (ToSchema)
import Data.String (fromString)
import Data.Text qualified as T
import GHC.Generics (Generic)
import HelloWorld.Contract (contract)
import Network.HTTP.Client (
  defaultManagerSettings,
  newManager,
 )
import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.Config qualified as CI
import Plutus.ChainIndex.Logging qualified as ChainIndex.Logging
import Plutus.ChainIndex.Types (Point (PointAtGenesis))
import Plutus.PAB.App (StorageBackend (BeamSqliteBackend))
import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (SomeBuiltin),
 )
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWithOpts)
import Plutus.PAB.Run.Command (ConfigCommand (Migrate, PABWebserver))
import Plutus.PAB.Run.CommandParser (AppOpts (..))
import Plutus.PAB.Types (Config (..), DbConfig (..))
import Plutus.PAB.Types qualified as PAB.Config
import Prettyprinter (Pretty (..), viaShow)
import Servant.Client (BaseUrl (..), Scheme (..))
import Servant.Client qualified as SC
import System.FilePath ((</>))
import Test.Integration.Faucet qualified as Faucet
import Test.Integration.Framework.DSL (fixturePassphrase)

newtype ChainIndexPort = ChainIndexPort Int
  deriving (Show)

-- Initial value passed to the PAB when starting up the HelloWorld contract.
data HelloWorldContracts
  = HelloWorld
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON HelloWorldContracts where
  toJSON =
    Asn.genericToJSON
      Asn.defaultOptions
        { Asn.tagSingleConstructors = True
        }
instance FromJSON HelloWorldContracts where
  parseJSON =
    Asn.genericParseJSON
      Asn.defaultOptions
        { Asn.tagSingleConstructors = True
        }

instance Pretty HelloWorldContracts where
  pretty = viaShow

instance HasDefinitions HelloWorldContracts where
  getDefinitions = [HelloWorld]
  getSchema = const []
  getContract = \case
    HelloWorld -> SomeBuiltin contract

runPAB :: String -> Int -> FilePath -> CardanoNodeConn -> IO ()
runPAB walletHost walletPort dir socketPath = do
  void $
    async $ do
      walletUrl <- restoreWallets walletHost walletPort
      chainIndexPort <- launchChainIndex dir socketPath
      launchPAB dir walletUrl socketPath chainIndexPort

-- | Set up wallets
restoreWallets :: String -> Int -> IO BaseUrl
restoreWallets walletHost walletPort = do
  sleep 15
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl {baseUrlScheme = Http, baseUrlHost = walletHost, baseUrlPort = walletPort, baseUrlPath = ""}
      clientEnv = SC.mkClientEnv manager baseUrl
      mnemonic :: ApiMnemonicT '[15, 18, 21, 24] = ApiMnemonicT $ SomeMnemonic $ head Faucet.seqMnemonics
      wpData =
        Wallet.Types.WalletPostData
          Nothing
          mnemonic
          Nothing
          (ApiT $ WalletName "plutus-wallet")
          (ApiT $ Passphrase $ fromString $ T.unpack fixturePassphrase)
      walletAcc = WalletOrAccountPostData {postData = Left wpData}
  result <- flip SC.runClientM clientEnv $ WalletClient.postWallet WalletClient.walletClient walletAcc
  case result of
    Left err -> do
      putStrLn "restoreWallet failed"
      putStrLn $ "Error: " <> show err
      putStrLn "restoreWallet: trying again in 30s"
      sleep 15
      restoreWallets walletHost walletPort
    Right (ApiWallet (ApiT i) _ _ _ _ _ _ _ _) -> do
      putStrLn $ "Restored wallet: " <> show i
      putStrLn $ "Passphrase: " <> T.unpack fixturePassphrase
      return baseUrl

launchPAB :: FilePath -> BaseUrl -> CardanoNodeConn -> ChainIndexPort -> IO ()
launchPAB dir walletUrl socketPath (ChainIndexPort chainIndexPort) = do
  void . async $ runOpts' Migrate
  sleep 2
  void . async $ runOpts' PABWebserver
  where
    cfg = Just $ mkPabConfig socketPath dir chainIndexPort walletUrl

    runOpts' cmd' = runWithOpts @HelloWorldContracts Builtin.handleBuiltin cfg appOpts {cmd = cmd'}

-- | Launch the chain index in a separate thread.
launchChainIndex :: FilePath -> CardanoNodeConn -> IO ChainIndexPort
launchChainIndex dir socketPath = do
  config <- ChainIndex.Logging.defaultConfig
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        CI.defaultConfig
          & CI.socketPath .~ nodeSocketFile socketPath
          & CI.dbPath .~ dbPath
          & CI.networkId .~ CAPI.Mainnet
  void . async $ void $ ChainIndex.runMain config chainIndexConfig
  return $ ChainIndexPort $ chainIndexConfig ^. CI.port

appOpts :: AppOpts
appOpts =
  AppOpts
    { minLogLevel = Nothing
    , logConfigPath = Nothing
    , configPath = Nothing
    , passphrase = Just fixturePassphrase
    , rollbackHistory = Nothing
    , resumeFrom = PointAtGenesis
    , runEkgServer = False
    , storageBackend = BeamSqliteBackend
    , cmd = PABWebserver
    }

mkPabConfig :: CardanoNodeConn -> FilePath -> Int -> BaseUrl -> Config
mkPabConfig socketPath dbPath ciPort walletUrl =
  PAB.Config.defaultConfig
    { nodeServerConfig = mkPABServerConfig (CNode.nodeSocketFile socketPath)
    , dbConfig = mkDbConfig dbPath
    , chainIndexConfig = mkChainIndexConfig ciPort
    , walletServerConfig = mkWalletConfig walletUrl
    }

mkPABServerConfig :: FilePath -> PABServerConfig
mkPABServerConfig socketPath =
  def
    { pscSocketPath = socketPath
    , pscNodeMode = AlonzoNode
    , pscNetworkId = NetworkIdWrapper CAPI.Mainnet
    }

mkDbConfig :: FilePath -> DbConfig
mkDbConfig dir =
  def
    { dbConfigFile = T.pack (dir </> "plutus-pab.db")
    }

mkChainIndexConfig :: Int -> ChainIndexConfig
mkChainIndexConfig port =
  def
    { ciBaseUrl = ChainIndexUrl baseUrl
    }
  where
    baseUrl = BaseUrl Http "localhost" port ""

mkWalletConfig :: BaseUrl -> Wallet.WalletConfig
mkWalletConfig walletUrl = Wallet.defaultWalletConfig & updateUrl
  where
    updateUrl = Wallet.walletSettingsL . Wallet.baseUrlL .~ Wallet.WalletUrl walletUrl

sleep :: Int -> IO ()
sleep n = CC.threadDelay $ n * 1_000_000
