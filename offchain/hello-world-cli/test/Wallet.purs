module Test.Wallet
  (makeWallet
  ) where

import Contract.Prelude
import Wallet.Key(KeyWallet)
import Contract.Wallet.KeyFile(privatePaymentKeyToFile,privateStakeKeyToFile)
import Wallet.Key(keyWalletPrivatePaymentKey,keyWalletPrivateStakeKey)
import Node.FS.Aff
  (readTextFile
  ,writeTextFile
  ,unlink
  )
import Node.Encoding (Encoding(UTF8))
import Aeson (encodeAeson)

makeWallet :: String -> String -> KeyWallet -> Aff Unit
makeWallet dir name wallet = do
  let cfgName = dir <> name <>  "-cfg.json"
  let walletName = dir <> name <> "-wallet.skey"
  let stakeName = dir <> name <> "-staking.skey"
  writeTextFile UTF8 cfgName $ encodeAeson >>>show $
    { walletPath : walletName
    , stakingPath : stakeName
      -- TODO this needs to handle nothing case
    , network : "Testnet" -- TODO this is wrong
    }
  privatePaymentKeyToFile walletName $ keyWalletPrivatePaymentKey wallet
  void $ traverse (privateStakeKeyToFile stakeName) $ keyWalletPrivateStakeKey wallet
