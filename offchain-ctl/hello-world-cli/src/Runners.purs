module Runners
  (runCLI
  ) where

-- Contract
import Contract.Prelude
import Contract.Monad
  ( DefaultContractConfig
  , Contract
  , runContract
  , runContract_
  , configWithLogLevel
  , liftContractAffM
  , liftContractM
  )
import Contract.Utxos(getUtxo,getWalletBalance)
import Contract.PlutusData(getDatumByHash)
import Contract.Wallet.KeyFile(mkKeyWalletFromFiles)
import Contract.Scripts (validatorHash)

-- Node
import Node.FS.Aff
  (readTextFile
  ,writeTextFile
  ,unlink
  )
import Node.FS.Sync(exists)
import Node.Encoding (Encoding(UTF8))
import Node.Process(exit)
import Effect.Exception(throw)
import Simple.JSON(readJSON,writeJSON)

-- Types
import Data.UInt as U
import Data.BigInt as Big
import Data.Tuple.Nested((/\))
import Data.Log.Level(LogLevel(Trace))
import Types.ByteArray (byteArrayToHex,hexToByteArrayUnsafe)
import Types.Datum(Datum(Datum))
import Types.PlutusData(PlutusData(Integer))
import Types.Transaction (TransactionInput(TransactionInput), TransactionHash(TransactionHash))
import Plutus.Types.Transaction(TransactionOutput(TransactionOutput))
import Plutus.Types.Value(flattenValue)
import Serialization.Address (NetworkId(TestnetId,MainnetId))

-- Local
import Api
  (helloScript
  ,sendDatumToScript
  ,setDatumAtScript
  ,redeemFromScript
  )
import Types
  (Command(..)
  ,Conf(..)
  ,CliState(..)
  ,SubCommand(..)
  ,ParsedOptions(..)
  ,ParsedConf
  ,FileState
  )


runCLI :: ParsedOptions -> Aff Unit
runCLI opts = readConfig opts >>= runCmd

readConfig :: ParsedOptions -> Aff Command
readConfig (ParsedOptions o)= do
  confTxt <- readTextFile UTF8 o.configFile
  conf' <- throwE $ readJSON confTxt
  conf <- throwE $ lookupNetwork conf'
  pure $ Command
    {subCommand: o.subCommand
    ,statePath: o.statePath
    ,conf:conf
    }

lookupNetwork :: ParsedConf -> Either String Conf
lookupNetwork p =
  let network = case p.network of
                  "Testnet" -> Right TestnetId
                  "Mainnet" -> Right MainnetId
                  n -> Left n
  in case network of
    Right net -> Right $ Conf p{network=net}
    Left name -> Left $ "unknown network: " <> name

throwE :: forall a b. Show a => Either a b -> Aff b
throwE (Left a) = liftEffect $ throw $ show a
throwE (Right b) = pure b

runCmd :: Command -> Aff Unit
runCmd (Command {conf,statePath,subCommand}) = do
  cfg <- makeConfig conf
  case subCommand of
    Lock {contractParam:param,initialDatum:init} -> do
      stateExists <- liftEffect $ exists statePath
      when stateExists $ do
        log "Can't use lock when state file already exists"
        liftEffect $ exit (0-1) -- afaict you have to do this?
      state <- runContract cfg $ do
        validator <- helloScript param
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        txid <- sendDatumToScript init vhash
        pure $ State {param,lastOutput:txid}
      writeState statePath state
    Increment -> do
      (State state) <- readState statePath
      newState <- runContract cfg $ do
        validator <- helloScript state.param
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        oldDatum <- getDatumFromState $ State state
        let newDatum = oldDatum + state.param
        txid <- setDatumAtScript newDatum vhash validator state.lastOutput
        pure $ State $ state{lastOutput=txid}
      writeState statePath newState
    End -> do
      (State state) <- readState statePath
      runContract_ cfg $ do
        validator <- helloScript state.param
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        redeemFromScript vhash validator state.lastOutput
      clearState statePath
    Querry -> do
      (State state) <- readState statePath
      (datum /\ bal) <- runContract cfg $ do
        datum <- getDatumFromState $ State state
        bal <- getWalletBalance
          >>= liftContractM "wallet balance failed"
        pure $ datum /\ bal
      log $ "Contract param:" <> show state.param
      log $ "Current datum:" <> show datum
      let TransactionInput out = state.lastOutput
      let TransactionHash hash = out.transactionId
      log $ "Last txid: https://testnet.cardanoscan.io/transaction/" <> byteArrayToHex hash
      log $ "txid index: " <> show out.index
      log "wallet bal: "
      for_ (flattenValue bal)
        $ \(cs/\tn/\amt) -> do
          log $ "  " <> (Big.toString amt) <> " of: "
          log $ "    " <> show cs <> "," <> show tn
  log "finished"
  liftEffect $ exit 1
  {- imo this exit shouldn't be needed
   - but the odc doesn't exit on its own
   - we will ask ctl about it
   -}

getDatumFromState :: CliState -> Contract () Int
getDatumFromState (State state) = do
  TransactionOutput utxo <- getUtxo state.lastOutput
    >>= liftContractM "couldn't find utxo"
  oldDatum <-
    utxo.dataHash
    # liftContractM "utxo had not datum hash"
    >>= getDatumByHash
    >>= liftContractM "Couldn't find datum by hash"
  asBigInt <- liftContractM "datum wasn't an integer" $ case oldDatum of
    Datum (Integer n) -> Just n
    _ -> Nothing
  liftContractM "Datum was actually big. We should support this but currently don't" $ Big.toInt asBigInt

writeState :: String -> CliState -> Aff Unit
writeState statePath s = do
  writeTextFile UTF8 statePath $ writeJSON $ logState s

readState :: String -> Aff CliState
readState statePath = do
  stateExists <- liftEffect $ exists statePath
  unless stateExists $ do
    log "State file could not be read because it doesn't exist"
    liftEffect $ exit (0-1) -- afaict you have to do this?
  stateTxt <- readTextFile UTF8 statePath
  (partial :: FileState) <- throwE $ readJSON stateTxt
  pure $ State $
    {param:partial.param
    ,lastOutput:parseTxId partial.lastOutput
    }

logState :: CliState -> FileState
logState (State {param,lastOutput}) = {param,lastOutput:logTxId lastOutput}

logTxId :: TransactionInput -> {index :: Int , transactionId :: String}
logTxId (TransactionInput {index,transactionId:TransactionHash bytes})
  = {index:U.toInt index,transactionId:byteArrayToHex bytes}

parseTxId :: {index :: Int,transactionId :: String} -> TransactionInput
parseTxId {index,transactionId}
  = TransactionInput
    {index:U.fromInt index,transactionId:TransactionHash $ hexToByteArrayUnsafe transactionId}

clearState :: String -> Aff Unit
clearState = unlink

makeConfig :: Conf -> Aff DefaultContractConfig
makeConfig
  (Conf{walletPath,stakingPath,network}) = do
  wallet <- mkKeyWalletFromFiles walletPath $ Just stakingPath
  configWithLogLevel network wallet Trace


