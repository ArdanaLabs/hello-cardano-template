module HelloWorld.Cli.Runners
  ( runCli
  ) where

import Contract.Prelude

import Aeson (decodeAeson, parseJsonStringToAeson, encodeAeson)
import Contract.Address (NetworkId(..))
import Contract.Config (testnetConfig)
import Contract.Monad (ConfigParams, runContract)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Value (flattenValue)
import Contract.Wallet (privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Data.BigInt as Big
import Data.String.CodeUnits (lastIndexOf, take)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested ((/\))
import Data.UInt as U
import Effect.Exception (throw)
import HelloWorld.Api (initialize, increment, redeem, query)
import HelloWorld.Cli.Types (CliState(..), Command(..), Conf(..), FileState, Options(..), ParsedConf, ParsedOptions(..), WalletConf(..))
import HsmWallet (makeHsmWallet)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, writeTextFile, unlink)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Util (getTxScanUrl)

runCli :: ParsedOptions -> Aff Unit
runCli opts = readConfig opts >>= runCmd

readConfig :: ParsedOptions -> Aff Options
readConfig (ParsedOptions o) = do
  confTxt <- readTextFile UTF8 o.configFile
  let
    dir = case lastIndexOf (Pattern "/") o.configFile of
      Just n -> take (n + 1) o.configFile
      Nothing -> ""
  conf' <- throwE =<< decodeAeson <$> throwE (parseJsonStringToAeson confTxt)
  conf <- lookupConf dir conf'
  pure $ Options
    { command: o.command
    , statePath: o.statePath
    , conf: conf
    , ctlPort: o.ctlPort
    , ogmiosPort: o.ogmiosPort
    , odcPort: o.odcPort
    }

lookupConf :: FilePath -> ParsedConf -> Aff Conf
lookupConf dir p = do
  network <- case p.network of
    "Testnet" -> pure TestnetId
    "Mainnet" -> pure MainnetId
    n -> liftEffect $ throw $ "unknown network:" <> show n
  wallet <- case p.wallet of
    KeyWalletFiles { walletPath, stakingPath } -> do
      key <- privatePaymentKeyFromFile $ dir <> walletPath
      mstake <- traverse privateStakeKeyFromFile $ (dir <> _) <$> stakingPath
      pure $ privateKeysToKeyWallet key mstake
    YubiHSM _ -> makeHsmWallet
  pure $ Conf { wallet, network }

throwE :: forall a b. Show a => Either a b -> Aff b
throwE (Left a) = liftEffect $ throw $ show a
throwE (Right b) = pure b

runCmd :: Options -> Aff Unit
runCmd (Options { conf, statePath, command, ctlPort, ogmiosPort, odcPort }) = do
  let cfg' = toConfigParams conf
  -- update ports from config
  let
    cfg = cfg'
      { ctlServerConfig = case cfg'.ctlServerConfig of
          Nothing -> Nothing
          Just serverConfig -> case ctlPort of
            Nothing -> Just serverConfig
            Just port -> Just $ serverConfig { port = port }
      , ogmiosConfig { port = fromMaybe cfg'.ogmiosConfig.port ogmiosPort }
      , datumCacheConfig { port = fromMaybe cfg'.datumCacheConfig.port odcPort }
      }
    wallet = (unwrap conf).wallet
  case command of
    Lock { contractParam: param, initialDatum: init } -> do
      stateExists <- liftEffect $ exists statePath
      when stateExists $ do
        liftEffect $ throw "Can't use lock when state file already exists"
      lastOutput <- runContract cfg $ withKeyWallet wallet $ initialize param init
      writeState statePath $ State { param, lastOutput }
    Increment -> do
      (State state) <- readState statePath
      lastOutput <- runContract cfg $ withKeyWallet wallet $ do
        increment state.param state.lastOutput
      writeState statePath $ State { param: state.param, lastOutput }
    Unlock -> do
      (State state) <- readState statePath
      void <<< runContract cfg $ withKeyWallet wallet $ redeem state.param state.lastOutput
      clearState statePath
    Query -> do
      (State state) <- readState statePath
      (datum /\ bal) <- runContract cfg $ withKeyWallet wallet $ query state.lastOutput
      log $ "Contract param:" <> show state.param
      log $ "Current datum:" <> show datum
      let TransactionInput out = state.lastOutput
      let Conf { network: network } = conf
      --let TransactionHash hash = out.transactionId
      log $ "Last txid: " <> getTxScanUrl network state.lastOutput
      log $ "txid index: " <> show out.index
      log "wallet bal: "
      for_ (flattenValue bal)
        $ \(cs /\ tn /\ amt) -> do
            log $ "  " <> (Big.toString amt) <> " of: "
            log $ "    " <> show cs <> "," <> show tn
  log "finished"

writeState :: String -> CliState -> Aff Unit
writeState statePath s = do
  writeTextFile UTF8 statePath $ s # logState # encodeAeson # show

readState :: String -> Aff CliState
readState statePath = do
  stateExists <- liftEffect $ exists statePath
  unless stateExists $ do
    liftEffect $ throw "State file could not be read because it doesn't exist"
  stateTxt <- readTextFile UTF8 statePath
  (partial :: FileState) <- throwE =<< decodeAeson <$> throwE (parseJsonStringToAeson stateTxt)
  pure $ State $
    { param: partial.param
    , lastOutput: parseTxId partial.lastOutput
    }

logState :: CliState -> FileState
logState (State { param, lastOutput }) = { param, lastOutput: logTxId lastOutput }

logTxId :: TransactionInput -> { index :: Int, transactionId :: String }
logTxId (TransactionInput { index, transactionId: TransactionHash bytes }) = { index: U.toInt index, transactionId: byteArrayToHex bytes }

parseTxId :: { index :: Int, transactionId :: String } -> TransactionInput
parseTxId { index, transactionId } = TransactionInput
  { index: U.fromInt index, transactionId: TransactionHash $ hexToByteArrayUnsafe transactionId }

clearState :: String -> Aff Unit
clearState = unlink

toConfigParams :: Conf -> ConfigParams ()
toConfigParams
  (Conf { network }) =
  testnetConfig { walletSpec = Nothing, networkId = network }

