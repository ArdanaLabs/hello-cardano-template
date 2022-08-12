module HelloWorld.Test.E2E.Env
  ( chromeExe
  , helloWorldBrowserIndex
  , namiExtension
  , namiWallet1
  , namiWallet2
  , namiWallet3
  , noRuntime
  ) where

chromeExe :: String
chromeExe = "CHROME_EXE"

namiExtension :: String
namiExtension = "NAMI_EXTENSION"

namiWallet1 :: String
namiWallet1 = "NAMI_TEST_WALLET_1"

namiWallet2 :: String
namiWallet2 = "NAMI_TEST_WALLET_2"

namiWallet3 :: String
namiWallet3 = "NAMI_TEST_WALLET_3"

noRuntime :: String
noRuntime = "NO_RUNTIME"

helloWorldBrowserIndex :: String
helloWorldBrowserIndex = "HELLO_WORLD_BROWSER_INDEX"