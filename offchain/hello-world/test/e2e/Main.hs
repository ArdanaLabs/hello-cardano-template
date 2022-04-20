module Main (main) where

import Cardano.Wallet.Primitive.Types qualified as CTypes
import Control.Concurrent qualified as CC
import Control.Concurrent.Async qualified as Async
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as Asn
import Data.Word (Word16)
import HelloWorld.LocalCluster qualified as LC
import Network.HTTP.Client qualified as HTTP
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (err))
import Plutus.PAB.Webserver.Client (PabClient)
import Plutus.PAB.Webserver.Client qualified as PABClient
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs), ContractInstanceClientState (cicCurrentState))
import Servant.Client (
  BaseUrl (..),
  ClientEnv (..),
  Scheme (..),
 )
import Servant.Client qualified as SC
import System.Exit qualified as SysEx
import Test.Syd (
  TestDefM,
  describe,
  itWithOuter,
  sydTest,
 )
import Test.Syd qualified as SydT
import Test.Syd.Def (SetupFunc (..))
import Test.Syd.Expectation qualified as SydEx
import Text.Read qualified as TR
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Emulator.Wallet qualified as WTypes
import Wallet.Types (ContractInstanceId)

type LocalClusterSpec = TestDefM '[TestArgs] () ()

data TestArgs = MkTestArgs
  { networkManager :: HTTP.Manager
  , wallet :: Wallet
  }

main :: IO ()
main =
  -- runCluster does not terminate, so we need to handle it with race_
  Async.race_ LC.runCluster $
    sydTest specWithArgs
  where
    specWithArgs = SydT.setupAroundAll setupTestArgs spec

spec :: LocalClusterSpec
spec = describe "End to End" $ do
  itWithOuter "Tests contract endpoints" $
    \MkTestArgs {networkManager, wallet} -> do
      let env = SC.mkClientEnv networkManager base
          pclient = PABClient.pabClient @_ @()
      contractId <- activatesContract env pclient wallet
      usesEndpoint "initialize" env pclient contractId
      usesEndpoint "increment" env pclient contractId
      usesEndpoint "read" env pclient contractId
  where
    base =
      BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = localhost
        , baseUrlPort = pabBackendPort
        , baseUrlPath = ""
        }

activatesContract :: ClientEnv -> PabClient String walletId -> Wallet -> IO ContractInstanceId
activatesContract env pclient wallet = do
  let activationArgs = ContractActivationArgs walletName (Just wallet)
      activateContract = PABClient.activateContract pclient activationArgs
  res <- SC.runClientM activateContract env
  case res of
    Left ce -> SydEx.expectationFailure $ "*** Failed to activate contract: " <> show ce
    Right cid -> do
      verifyStatus env pclient cid
      pure cid

usesEndpoint ::
  String ->
  ClientEnv ->
  PabClient String walletId ->
  ContractInstanceId ->
  IO ()
usesEndpoint endpoint env pclient contractId = do
  let instanceClient = PABClient.instanceClient pclient contractId
      params = Asn.toJSON ()
      req = PABClient.callInstanceEndpoint instanceClient endpoint params
  res <- SC.runClientM req env
  case res of
    Left ce -> reqErr ce
    Right _ -> verifyStatus env pclient contractId
  where
    errStr = "*** Failed on endpoint <" <> endpoint <> ">: "
    reqErr = SydEx.expectationFailure . (<>) errStr . show

{- | Verifies the contract status. This is necessary because the obvious way
 to check for errors -- i.e. if 'SC.runClientM' returns 'Left' -- is
 insufficient. For example, the PAB backend will happily return Right ()
 when we supply the wrong aeson params (e.g. 'Bool' when it's expecting
 ()).

 Luckily, these errors appear to be available on the status endpoint,
 hence this check.
-}
verifyStatus ::
  ClientEnv ->
  PabClient String walletId ->
  ContractInstanceId ->
  IO ()
verifyStatus env pclient contractId = do
  -- Pretty hacky, but evidently we need a delay whenever one endpoint must
  -- complete before the next ones can be called. Thus we call one before
  -- this function, which should be checked after each contract step
  sleepSeconds 1
  let instanceClient = PABClient.instanceClient pclient contractId
      req = PABClient.getInstanceStatus instanceClient
  res <- SC.runClientM req env
  case res of
    Left ce -> reqErr ce
    Right st ->
      let mErr = err $ cicCurrentState st
       in maybe (pure ()) statusErr mErr
  where
    reqErr = SydEx.expectationFailure . (<>) "*** Failed to get status: " . show
    statusErr = SydEx.expectationFailure . (<>) "*** Status error: " . show

setupTestArgs :: SetupFunc TestArgs
setupTestArgs = do
  netManager <- setupManager
  wallet <- setupWallet
  setupCluster netManager wallet

setupWallet :: SetupFunc Wallet
setupWallet = liftIO $ do
  case mWallet of
    Nothing -> SysEx.die "*** Error creating wallet digest"
    Just w -> pure w

setupManager :: SetupFunc HTTP.Manager
setupManager = liftIO (HTTP.newManager HTTP.defaultManagerSettings)

setupCluster :: HTTP.Manager -> Wallet -> SetupFunc TestArgs
setupCluster manager wallet = liftIO $ do
  clusterUp <- LC.waitCluster manager clusterTimeout localhost walletPort pabBackendPort
  if clusterUp
    then pure $ MkTestArgs manager wallet
    else SysEx.die $ "*** Cluster failed to start after " <> show clusterTimeout <> " seconds"

{- | Name for our test wallet. This must match the data constructor for the
 contract we are running. I.e. in HelloWorld.PAB we have

 @
 data HelloWorldContracts = HelloWorld -- <- matches this
 @
-}
walletName :: String
walletName = "HelloWorld"

-- | Test wallet with hardcoded digest.
mWallet :: Maybe Wallet
mWallet = WTypes.Wallet (Just walletName) . WTypes.WalletId . CTypes.WalletId <$> digest
  where
    digest = TR.readMaybe "2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"

clusterTimeout :: Word16
clusterTimeout = 180

localhost :: String
localhost = "127.0.0.1"

walletPort :: Int
walletPort = 46493

pabBackendPort :: Int
pabBackendPort = 9080

-- | Sleeps for the given number of seconds
sleepSeconds :: Int -> IO ()
sleepSeconds = CC.threadDelay . (* 1_000_000)
