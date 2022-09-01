module Faucet(topupKeyWallet) where

import Contract.Prelude
import Wallet.Key(KeyWallet(KeyWallet))
import Serialization.Address (NetworkId(TestnetId, MainnetId))
import Node.ChildProcess (defaultExecSyncOptions, execSync)

topupKeyWallet :: String -> Aff Unit
topupKeyWallet addr = do
  let url = faucetUrl <> addr <> "?apiKey=" <> faucetApiKey
  liftEffect $ void $ execSync ("curl -XPOST " <> url) defaultExecSyncOptions

faucetUrl :: String
faucetUrl = "https://faucet.cardano-testnet.iohkdev.io/send-money/"

faucetApiKey :: String
faucetApiKey = "r8m9YXmqCkFWDDZ2540IJaJwr1JBxqXB"
