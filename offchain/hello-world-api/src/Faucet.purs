module Faucet (topup) where

import Contract.Prelude
import Node.ChildProcess (defaultExecSyncOptions, execSync)

topup :: String -> Effect Unit
topup addr = do
  let url = faucetUrl <> addr <> "?apiKey=" <> faucetApiKey
  let cmd = "curl -XPOST " <> url
  log $ "running:" <> cmd
  void $ execSync cmd defaultExecSyncOptions

faucetUrl :: String
faucetUrl = "https://faucet.cardano-testnet.iohkdev.io/send-money/"

faucetApiKey :: String
faucetApiKey = "r8m9YXmqCkFWDDZ2540IJaJwr1JBxqXB"
