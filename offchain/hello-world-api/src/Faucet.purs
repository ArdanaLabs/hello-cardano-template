module Faucet (topup) where

import Contract.Prelude
import Wallet.Key (KeyWallet(KeyWallet))
import Serialization.Address (NetworkId(TestnetId, MainnetId))
import Node.ChildProcess (defaultExecSyncOptions, execSync)

topup :: String -> Effect Unit
topup addr = do
  let url = faucetUrl <> addr <> "?apiKey=" <> faucetApiKey
  void $ execSync ("curl -XPOST " <> url) defaultExecSyncOptions

faucetUrl :: String
faucetUrl = "https://faucet.cardano-testnet.iohkdev.io/send-money/"

faucetApiKey :: String
faucetApiKey = "r8m9YXmqCkFWDDZ2540IJaJwr1JBxqXB"
