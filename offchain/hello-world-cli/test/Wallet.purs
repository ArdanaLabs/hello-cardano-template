module Test.Wallet
  (makeWallet
  ,rmWallet
  ) where

import Aeson(decodeAeson,parseJsonStringToAeson,encodeAeson)
import Contract.Prelude
import Contract.Wallet.KeyFile(privatePaymentKeyToFile,privateStakeKeyToFile)
import Data.String(take,lastIndexOf,Pattern(Pattern))
import Effect.Exception(throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff
  (readTextFile
  ,writeTextFile
  ,unlink
  )
import Wallet.Key(KeyWallet)
import Wallet.Key(keyWalletPrivatePaymentKey,keyWalletPrivateStakeKey)

import HelloWorld.Cli.Types (ParsedConf)

makeWallet :: String -> String -> KeyWallet -> Aff String
makeWallet dir name wallet = do
  let cfgName = dir <> name <>  "-cfg.json"
  let walletName = name <> "-wallet.skey"
  let stakeName = name <> "-staking.skey"
  writeTextFile UTF8 cfgName $ encodeAeson >>>show $
    { walletPath : walletName
    , stakingPath : stakeName <$ keyWalletPrivateStakeKey wallet
    , network : "Testnet" -- TODO this is wrong
    }
  privatePaymentKeyToFile walletName $ keyWalletPrivatePaymentKey wallet
  void $ traverse (privateStakeKeyToFile stakeName) $ keyWalletPrivateStakeKey wallet
  pure cfgName

rmWallet :: String -> Aff Unit
rmWallet path = do
  confTxt <- readTextFile UTF8 path
  let dir = case lastIndexOf (Pattern "/") path of
              Just n -> take (n+1) path
              Nothing -> ""
  (conf :: ParsedConf) <- throwE =<< decodeAeson <$> throwE (parseJsonStringToAeson confTxt)
  unlink path
  unlink $ dir <> conf.walletPath
  void $ traverse unlink ((dir <> _) <$> conf.stakingPath)

throwE :: forall a b. Show a => Either a b -> Aff b
throwE (Left a) = liftEffect $ throw $ show a
throwE (Right b) = pure b
