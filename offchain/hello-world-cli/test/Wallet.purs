module Test.Wallet
  (makeWallet
  ) where

import Aeson (encodeAeson)
import Contract.Prelude
import Contract.Wallet.KeyFile(privatePaymentKeyToFile,privateStakeKeyToFile)
import Wallet.Key(keyWalletPrivatePaymentKey,keyWalletPrivateStakeKey)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff
  (readTextFile
  ,writeTextFile
  ,unlink
  )
import Wallet.Key(KeyWallet)

makeWallet :: String -> String -> KeyWallet -> Aff String
makeWallet dir name wallet = do
  let cfgName = dir <> name <>  "-cfg.json"
  let walletName = dir <> name <> "-wallet.skey"
  let stakeName = dir <> name <> "-staking.skey"
  writeTextFile UTF8 cfgName $ encodeAeson >>>show $
    { walletPath : walletName
    , stakingPath : stakeName <$ keyWalletPrivateStakeKey wallet
    , network : "Testnet" -- TODO this is wrong
    }
  privatePaymentKeyToFile walletName $ keyWalletPrivatePaymentKey wallet
  void $ traverse (privateStakeKeyToFile stakeName) $ keyWalletPrivateStakeKey wallet
  pure cfgName
