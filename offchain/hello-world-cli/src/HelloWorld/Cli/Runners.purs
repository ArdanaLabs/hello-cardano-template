module HelloWorld.Cli.Runners
  ( runCli
  ) where

-- Contract
import Contract.Prelude
import Contract.Config (testnetConfig)
import Contract.Monad (ConfigParams, runContract)

-- Node
import Node.FS.Aff
  ( readTextFile
  , writeTextFile
  , unlink
  )
import Node.FS.Sync (exists)
import Node.Encoding (Encoding(UTF8))
import Effect.Exception (throw)
import Aeson (decodeAeson, parseJsonStringToAeson, encodeAeson)

-- Types
import Data.UInt as U
import Data.BigInt as Big
import Data.String.CodeUnits (lastIndexOf, take)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested ((/\))
import Types.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Types.Transaction (TransactionInput(TransactionInput), TransactionHash(TransactionHash))
import Plutus.Types.Value (flattenValue)
import Serialization.Address (NetworkId(TestnetId, MainnetId))
import Wallet.Spec
  ( WalletSpec(UseKeys)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , PrivateStakeKeySource(PrivateStakeKeyFile)
  )

-- Local
import HelloWorld.Api
  ( initialize
  , increment
  , redeem
  , query
  )
import Util (getTxScanUrl)
import HelloWorld.Cli.Types
  ( Command(..)
  , Conf(..)
  , CliState(..)
  , Options(..)
  , ParsedOptions(..)
  , ParsedConf
  , FileState
  )

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
  Conf conf <- throwE $ lookupNetwork conf'
  pure $ Options
    { command: o.command
    , statePath: o.statePath
    , conf: Conf conf
        { walletPath = dir <> conf.walletPath
        , stakingPath = (dir <> _) <$> conf.stakingPath
        }
    , ctlPort: o.ctlPort
    , ogmiosPort: o.ogmiosPort
    , odcPort: o.odcPort
    }

lookupNetwork :: ParsedConf -> Either String Conf
lookupNetwork p =
  let
    network = case p.network of
      "Testnet" -> Right TestnetId
      "Mainnet" -> Right MainnetId
      n -> Left n
  in
    case network of
      Right net -> Right $ Conf p { network = net }
      Left name -> Left $ "unknown network: " <> name

throwE :: forall a b. Show a => Either a b -> Aff b
throwE (Left a) = liftEffect $ throw $ show a
throwE (Right b) = pure b

runCmd :: Options -> Aff Unit
runCmd (Options { conf, statePath, command, ctlPort, ogmiosPort, odcPort }) = do
  let cfg' = toConfigParams conf
  -- update ports from config
  let
    cfg = cfg'
      { ctlServerConfig { port = fromMaybe cfg'.ctlServerConfig.port ctlPort }
      , ogmiosConfig { port = fromMaybe cfg'.ogmiosConfig.port ogmiosPort }
      , datumCacheConfig { port = fromMaybe cfg'.datumCacheConfig.port odcPort }
      }
  case command of
    Lock { contractParam: param, initialDatum: init } -> do
      stateExists <- liftEffect $ exists statePath
      when stateExists $ do
        liftEffect $ throw "Can't use lock when state file already exists"
      lastOutput <- runContract cfg $ initialize param init
      writeState statePath $ State { param, lastOutput }
    Increment -> do
      (State state) <- readState statePath
      lastOutput <- runContract cfg $ increment state.param state.lastOutput
      writeState statePath $ State { param: state.param, lastOutput }
    Unlock -> do
      (State state) <- readState statePath
      void <<< runContract cfg $ redeem state.param state.lastOutput
      clearState statePath
    Query -> do
      (State state) <- readState statePath
      (datum /\ bal) <- runContract cfg $ query state.lastOutput
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
  (Conf { walletPath, stakingPath, network }) =
  let
    wallet = UseKeys
      (PrivatePaymentKeyFile walletPath)
      (PrivateStakeKeyFile <$> stakingPath)
  in
    testnetConfig { walletSpec = Just wallet, networkId = network }

