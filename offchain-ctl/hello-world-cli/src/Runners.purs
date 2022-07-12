module Runners
  (runCmd
  -- todo these are exported to supress warnings
  ,readConfig
  ,makeConfig
  ) where

import Contract.Prelude

import Effect(Effect)
import Effect.Exception(throw)
import Types
  (Command(..)
  ,Conf(..)
  ,CliState(..)
  ,SubCommand(..)
  ,ParsedOptions(..)
  ,ParsedConf
  ,ParsedState
  )
import Contract.Monad
  ( DefaultContractConfig
  , launchAff_
  , runContract
  , runContract_
  , configWithLogLevel
  , liftContractAffM
  )
import Data.Log.Level(LogLevel(Trace))
import Contract.Wallet.KeyFile(mkKeyWalletFromFiles)
import Contract.Scripts (validatorHash)
import Node.FS.Aff
  (readTextFile
  ,writeTextFile
  ,unlink
  )
import Node.Encoding (Encoding(UTF8))
import Simple.JSON(readJSON)
import Serialization.Address (NetworkId(TestnetId,MainnetId))
import Api
  (helloScript
  ,sendDatumToScript
  ,setDatumAtScript
  ,redeemFromScript
  )

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
      state <- runContract cfg $ do
        validator <- helloScript param
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        txid <- sendDatumToScript init vhash
        pure $ State {param,datum:init,lastOutput:txid}
      writeState statePath state
    Increment -> do
      (State state) <- readState statePath
      newState <- runContract cfg $ do
        validator <- helloScript state.param
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        txid <- setDatumAtScript (state.param+state.datum) vhash validator state.lastOutput
        pure $ State $ state{lastOutput=txid}
      writeState statePath newState
    End -> do
      (State state) <- readState statePath
      runContract_ cfg $ do
        validator <- helloScript state.param
        vhash <- liftContractAffM "Couldn't hash validator" $ validatorHash validator
        redeemFromScript vhash validator state.lastOutput
      clearState statePath

writeState :: String -> CliState -> Aff Unit
writeState statePath (State o) = do
  writeTextFile UTF8 statePath $ show o

readState :: String -> Aff CliState
readState statePath = do
  stateTxt <- readTextFile UTF8 statePath
  readTxid <$> (throwE $ readJSON stateTxt)

readTxid :: ParsedState -> CliState
readTxid = undefined

clearState :: String -> Aff Unit
clearState = unlink

makeConfig :: Conf -> Aff DefaultContractConfig
makeConfig
  (Conf{walletPath,stakingPath,network}) = do
  wallet <- mkKeyWalletFromFiles walletPath $ Just stakingPath
  configWithLogLevel network wallet Trace


