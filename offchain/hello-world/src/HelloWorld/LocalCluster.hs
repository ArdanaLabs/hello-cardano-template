module HelloWorld.LocalCluster (
  runCluster,
  waitCluster,
) where

import Cardano.BM.Backend.EKGView qualified as EKG
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Data.Tracer (
  HasPrivacyAnnotation (..),
  HasSeverityAnnotation (..),
 )
import Cardano.BM.Plugin (loadPlugin)
import Cardano.CLI (
  LogOutput (..),
  Port,
  ekgEnabled,
  getEKGURL,
  getPrometheusURL,
  withLoggingNamed,
 )
import Cardano.Startup (
  installSignalHandlers,
  setDefaultFilePermissions,
  withUtf8Encoding,
 )
import Cardano.Wallet.Api.Client (WalletClient (..), walletClient)
import Cardano.Wallet.Api.Server (Listen (..))
import Cardano.Wallet.Api.Types (EncodeAddress (..))
import Cardano.Wallet.Logging (
  stdoutTextTracer,
  trMessageText,
 )
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant (..))
import Cardano.Wallet.Primitive.SyncProgress (SyncTolerance (..))
import Cardano.Wallet.Primitive.Types.Coin (Coin (..))
import Cardano.Wallet.Shelley (
  SomeNetworkDiscriminant (..),
  serveWallet,
  setupTracers,
  tracerSeverities,
 )
import Cardano.Wallet.Shelley.BlockchainSource (BlockchainSource (NodeSource))
import Cardano.Wallet.Shelley.Launch (withSystemTempDir)
import Cardano.Wallet.Shelley.Launch.Cluster (
  ClusterLog (..),
  Credential (..),
  RunningNode (..),
  localClusterConfigFromEnv,
  moveInstantaneousRewardsTo,
  oneMillionAda,
  sendFaucetFundsTo,
  testMinSeverityFromEnv,
  tokenMetadataServerFromEnv,
  walletMinSeverityFromEnv,
  withCluster,
 )
import Control.Arrow (first)
import Control.Concurrent qualified as CC
import Control.Lens
import Control.Monad (void, when)
import Control.Tracer (traceWith)
import Data.Functor (($>))
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Class (ToText (..))
import Data.Word (Word16)
import HelloWorld.PAB qualified as PAB
import Network.HTTP.Client (Manager)
import Plutus.PAB.Webserver.Client (PabClient (..), pabClient)
import Servant.Client (BaseUrl (..), Scheme (Http))
import Servant.Client qualified as SC
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.Timeout qualified as TO
import Test.Integration.Faucet (
  genRewardAccounts,
  mirMnemonics,
  shelleyIntegrationTestFunds,
 )

data LogOutputs = LogOutputs
  { loCluster :: [LogOutput]
  , loWallet :: [LogOutput]
  }

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup ::
  (FilePath -> LogOutputs -> IO a) ->
  IO a
withLocalClusterSetup action = do
  putStrLn "Starting PAB local cluster. Please make sure the SHELLEY_TEST_DATA environment variable is set to 'plutus-pab/local-cluster/cluster-data/cardano-node-shelley' in the plutus-apps repository."

  -- Handle SIGTERM properly
  installSignalHandlers (putStrLn "Terminated")

  -- Ensure key files have correct permissions for cardano-cli
  setDefaultFilePermissions

  -- Set UTF-8, regardless of user locale
  withUtf8Encoding $ do
    -- This temporary directory will contain logs, and all other data
    -- produced by the local test cluster.
    withSystemTempDir stdoutTextTracer "test-cluster" $ \dir -> do
      let logOutputs name minSev =
            [ LogToFile (dir </> name) (min minSev Info)
            , LogToStdStreams minSev
            ]

      lops <-
        LogOutputs
          <$> (logOutputs "cluster.log" <$> testMinSeverityFromEnv)
          <*> (logOutputs "wallet.log" <$> walletMinSeverityFromEnv)

      action dir lops

-- | Runs the "local cluster" i.e. cardano-node(s), wallet, and our PAB.
runCluster :: IO ()
runCluster = withLocalClusterSetup $ \dir lo@LogOutputs {loCluster} ->
  withLoggingNamed "cluster" loCluster $ \(_, (_, trCluster)) -> do
    let tr' = contramap MsgCluster $ trMessageText trCluster
    clusterCfg <- localClusterConfigFromEnv
    withCluster
      tr'
      dir
      clusterCfg
      (setupFaucet dir (trMessageText trCluster))
      (whenReady dir (trMessageText trCluster) lo)
  where
    setupFaucet dir trCluster (RunningNode socketPath _ _) = do
      traceWith trCluster MsgSettingUpFaucet
      let trCluster' = contramap MsgCluster trCluster
      let encodeAddresses = map (first (T.unpack . encodeAddress @ 'Mainnet))
      let accts = KeyCredential <$> concatMap genRewardAccounts mirMnemonics
      let rewards = (,Coin $ fromIntegral oneMillionAda) <$> accts

      sendFaucetFundsTo trCluster' socketPath dir $
        encodeAddresses shelleyIntegrationTestFunds

      moveInstantaneousRewardsTo trCluster' socketPath dir rewards

    whenReady dir trCluster LogOutputs {loWallet} (RunningNode socketPath block0 (gp, vData)) = do
      withLoggingNamed "cardano-wallet" loWallet $ \(sb, (cfg, tr)) -> do
        let walletHost = "127.0.0.1"
            walletPort = 46493

        PAB.runPAB walletHost walletPort dir socketPath

        ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)

        let tracers = setupTracers (tracerSeverities (Just Debug)) tr
        let db = dir </> "wallets"
        createDirectory db
        tokenMetadataServer <- tokenMetadataServerFromEnv

        prometheusUrl <-
          maybe
            "none"
            (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)
            <$> getPrometheusURL
        ekgUrl <-
          maybe
            "none"
            (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)
            <$> getEKGURL

        void $
          serveWallet
            (NodeSource socketPath vData)
            gp
            (SomeNetworkDiscriminant $ Proxy @ 'Mainnet)
            tracers
            (SyncTolerance 10)
            (Just db)
            Nothing
            (fromString walletHost)
            (ListenOnPort walletPort)
            Nothing
            Nothing
            tokenMetadataServer
            block0
            ( \u ->
                traceWith trCluster $
                  MsgBaseUrl
                    (T.pack . show $ u)
                    ekgUrl
                    prometheusUrl
            )

data TestsLog
  = MsgBaseUrl Text Text Text
  | MsgSettingUpFaucet
  | MsgCluster ClusterLog
  deriving (Show)

instance ToText TestsLog where
  toText = \case
    MsgBaseUrl walletUrl ekgUrl prometheusUrl ->
      mconcat
        [ "Wallet url: "
        , walletUrl
        , ", EKG url: "
        , ekgUrl
        , ", Prometheus url:"
        , prometheusUrl
        ]
    MsgSettingUpFaucet -> "Setting up faucet..."
    MsgCluster msg -> toText msg

instance HasPrivacyAnnotation TestsLog

instance HasSeverityAnnotation TestsLog where
  getSeverityAnnotation = \case
    MsgSettingUpFaucet -> Notice
    MsgBaseUrl {} -> Notice
    MsgCluster msg -> getSeverityAnnotation msg

{- | Returns 'True' if both the wallet and PAB services are started within
 the parameter timeout. Otherwise returns 'False'.
-}
waitCluster :: Manager -> Word16 -> String -> Int -> Int -> IO Bool
waitCluster netManager clusterTimeout walletAddress walletPort pabBackendPort = do
  putStrLn "*** Waiting for wallet..."
  walletIsUp <- walletUp netManager clusterTimeout walletAddress walletPort
  if not walletIsUp
    then do
      putStrLn $ "*** Wallet failed to start after " <> show clusterTimeout <> " seconds"
      pure False
    else do
      putStrLn "*** Wallet successfully started"
      putStrLn "*** Waiting for PAB..."
      pabIsUp <- pabUp netManager clusterTimeout pabBackendPort
      if not pabIsUp
        then do
          putStrLn $ "*** PAB failed to start after " <> show clusterTimeout <> " seconds"
          pure False
        else putStrLn "*** PAB successfully started" $> True

pabUp :: Manager -> Word16 -> Int -> IO Bool
pabUp netManager clusterTimeout pabPort = do
  let env = SC.mkClientEnv netManager base
  result <- retryIO clusterTimeout (pabHealthcheck env)
  pure $ case result of
    Just _ -> True
    Nothing -> False
  where
    pabHealthcheck e = do
      putStrLn "*** Verifying PAB healthcheck"
      res <- SC.runClientM (healthcheck (pabClient @String @String)) e
      case res of
        Left ce -> do
          putStrLn "*** PAB error"
          pure $ Left ce
        Right aws -> do
          putStrLn "*** PAB healthcheck succeeded"
          pure $ Right aws
    base =
      BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = "127.0.0.1"
        , baseUrlPort = pabPort
        , baseUrlPath = ""
        }

walletUp :: Manager -> Word16 -> String -> Int -> IO Bool
walletUp netManager clusterTimeout walletAddress walletPort = do
  let env = SC.mkClientEnv netManager base
  result <- retryIO clusterTimeout (getWallets env)
  pure $ case result of
    Just _ -> True
    Nothing -> False
  where
    getWallets e = do
      putStrLn "*** Attempting to connect to wallet"
      res <- SC.runClientM (listWallets walletClient) e
      case res of
        Left ce -> do
          putStrLn "*** Wallet error"
          pure $ Left ce
        Right aws -> do
          putStrLn "*** Wallet connected"
          pure $ Right aws
    base =
      BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = walletAddress
        , baseUrlPort = walletPort
        , baseUrlPath = ""
        }

retryIO :: Word16 -> IO (Either e a) -> IO (Maybe a)
retryIO timeout action = TO.timeout timeoutMicro loopAction
  where
    timeoutMicro = secondsToMicro $ word16ToInt timeout
    loopAction = do
      res <- action
      case res of
        Right x -> pure x
        Left _ -> do
          CC.threadDelay $ secondsToMicro 10
          loopAction

secondsToMicro :: Int -> Int
secondsToMicro = (* 1_000_000)

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral
