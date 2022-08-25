module Test.Wallet
  ( withPlutipWalletFile
  ) where

import Aeson (decodeAeson, parseJsonStringToAeson, encodeAeson)
import Contract.Prelude
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Contract.Wallet.KeyFile (privatePaymentKeyToFile, privateStakeKeyToFile)
import Data.BigInt (BigInt)
import Data.UInt as UInt
import Data.String (take, lastIndexOf, Pattern(Pattern))
import Effect.Exception (throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff
  ( readTextFile
  , writeTextFile
  , unlink
  )
import Serialization.Address (NetworkId(TestnetId, MainnetId))
import Wallet.Key (KeyWallet, keyWalletPrivatePaymentKey, keyWalletPrivateStakeKey)

import HelloWorld.Cli.Types (ParsedConf)

-- TODO if it becomes usefull this could be made to work with optionally many wallets
-- but that's non-trivial and we don't need it yet
withPlutipWalletFile :: forall a. PlutipConfig -> Array BigInt -> String -> (String -> String -> Aff a) -> Aff a
withPlutipWalletFile config vals walletDir f = withPlutipContractEnv config vals \env wallet -> do
  w <- makeWallet (unwrap env).config.networkId walletDir "plutip" wallet
  let
    portArgs =
      " --ctl-port " <> show (UInt.toInt (unwrap env).config.ctlServerConfig.port)
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
    { walletPath: walletName
    , stakingPath: stakeName <$ keyWalletPrivateStakeKey wallet
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
  unlink $ dir <> conf.walletPath
  void $ traverse unlink ((dir <> _) <$> conf.stakingPath)

throwE :: forall a b. Show a => Either a b -> Aff b
throwE (Left a) = liftEffect $ throw $ show a
throwE (Right b) = pure b
