module Test.Wallet
  ( withPlutipWalletFile
  , withFundedServerWalletFile
  ) where

import Contract.Prelude

import Aeson (decodeAeson, parseJsonStringToAeson, encodeAeson)
import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..))
import Contract.Credential (StakingCredential(..))
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, runContractInEnv)
import Contract.PlutusData (PlutusData)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Contract.TxConstraints (TxConstraint(..), TxConstraints, singleton)
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (getWalletAddress, withKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyToFile, privateStakeKeyToFile)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.String (take, lastIndexOf, Pattern(Pattern))
import Data.UInt as UInt
import Effect.Exception (throw)
import HelloWorld.Cli.Types (ParsedConf, WalletConf(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, writeTextFile, unlink)
import Plutus.Types.Address (Address(..))
import Plutus.Types.Credential (Credential(..))
import Serialization.Address (NetworkId(TestnetId, MainnetId))
import ServerWallet (makeServerWallet)
import Util (buildBalanceSignAndSubmitTx, maxWait, waitForTx)
import Wallet.Key (KeyWallet, keyWalletPrivatePaymentKey, keyWalletPrivateStakeKey)

withFundedServerWalletFile :: forall a. PlutipConfig -> Array BigInt -> String -> (String -> String -> Aff a) -> Aff a
withFundedServerWalletFile config vals fixturesDir f = withPlutipContractEnv config vals \env wallet -> do
  runContractInEnv env $ do
    logInfo' "starting wallet funding phas"
    serverWallet <- liftAff $ makeServerWallet "SIGNING_CMD"
    adr <- withKeyWallet serverWallet getWalletAddress >>= liftContractM "no wallet"
    (key /\ skey) <- liftContractM "bad adr" =<< case adr of
      Address { addressCredential: PubKeyCredential key, addressStakingCredential: mskey } -> do
        skey <- case mskey of
          Nothing -> pure Nothing
          Just (StakingHash (PubKeyCredential skey)) -> pure $ Just skey
          _ -> liftEffect $ throw "bad staking credential"
        pure $ Just $ key /\ skey
      _ -> pure Nothing

    let
      lookups :: Lookups.ScriptLookups PlutusData
      lookups = mempty

      constraints :: TxConstraints Unit Unit
      constraints = singleton (MustPayToPubKeyAddress (PaymentPubKeyHash key) (StakePubKeyHash <$> skey) Nothing Nothing (lovelaceValueOf $ BigInt.fromInt 30_000_000))
    txid <-withKeyWallet wallet $ buildBalanceSignAndSubmitTx lookups constraints
    logInfo' "wallet funding finished"
    res <- waitForTx maxWait adr txid >>= liftContractM "time out"
    logInfo' $ "res: " <> show res
    logInfo' $ "server wallet adr: " <> show adr
    bal <- withKeyWallet serverWallet $ getWalletBalance >>= liftContractM "no wallet"
    logInfo' $ "post funding bal: " <> show bal
    txs <- withKeyWallet serverWallet $ getWalletUtxos >>= liftContractM "no wallet"
    logInfo' $ "server wallet txs: " <> show txs
  let
    portArgs =
      fromMaybe "" ((" --ctl-port " <> _) <<< show <<< UInt.toInt <$> ((unwrap env).config.ctlServerConfig <#> _.port))
        <> " --ogmios-port "
        <> show (UInt.toInt (unwrap env).config.ogmiosConfig.port)
        <> " --odc-port "
        <> show (UInt.toInt (unwrap env).config.datumCacheConfig.port)
        <> " "
  f portArgs (" " <> fixturesDir <> "serverWalletCfg.json ")

-- TODO if it becomes usefull this could be made to work with optionally many wallets
-- but that's non-trivial and we don't need it yet
withPlutipWalletFile :: forall a. PlutipConfig -> Array BigInt -> String -> (String -> String -> Aff a) -> Aff a
withPlutipWalletFile config vals walletDir f = withPlutipContractEnv config vals \env wallet -> do
  w <- makeWallet (unwrap env).config.networkId walletDir "plutip" wallet
  let
    portArgs =
      fromMaybe "" ((" --ctl-port " <> _) <<< show <<< UInt.toInt <$> ((unwrap env).config.ctlServerConfig <#> _.port))
        <> " --ogmios-port "
        <> show (UInt.toInt (unwrap env).config.ogmiosConfig.port)
        <> " --odc-port "
        <> show (UInt.toInt (unwrap env).config.datumCacheConfig.port)
        <> " "
  res <- f portArgs (" " <> w <> " ") -- spaces are convenient for passing it as an argument
  rmWallet w
  pure res

makeWallet :: NetworkId -> String -> String -> KeyWallet -> Aff String
makeWallet network dir name wallet = do
  let cfgName = dir <> name <> "-cfg.json"
  let walletName = name <> "-wallet.skey"
  let stakeName = name <> "-staking.skey"
  writeTextFile UTF8 cfgName $ encodeAeson >>> show $
    { wallet :
      { walletPath: walletName
      , stakingPath: stakeName <$ keyWalletPrivateStakeKey wallet
      }
    , network: case network of
        MainnetId -> "Mainnet"
        TestnetId -> "Testnet"
    }
  privatePaymentKeyToFile walletName $ keyWalletPrivatePaymentKey wallet
  void $ traverse (privateStakeKeyToFile stakeName) $ keyWalletPrivateStakeKey wallet
  pure cfgName

rmWallet :: String -> Aff Unit
rmWallet path = do
  confTxt <- readTextFile UTF8 path
  let
    dir = case lastIndexOf (Pattern "/") path of
      Just n -> take (n + 1) path
      Nothing -> ""
  (conf :: ParsedConf) <- throwE =<< decodeAeson <$> throwE (parseJsonStringToAeson confTxt)
  unlink path
  case conf.wallet of
    Files {walletPath,stakingPath} -> do
      unlink $ dir <> walletPath
      void $ traverse unlink ((dir <> _) <$> stakingPath)
    _ -> pure unit

throwE :: forall a b. Show a => Either a b -> Aff b
throwE (Left a) = liftEffect $ throw $ show a
throwE (Right b) = pure b
