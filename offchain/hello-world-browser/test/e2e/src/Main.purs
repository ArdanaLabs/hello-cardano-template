module HelloWorld.Test.E2E.Main where

import Contract.Prelude

import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import HelloWorld.Test.E2E.Env as Env
import HelloWorld.Test.E2E.Helpers (closeStaticServer, startStaticServer)
import HelloWorld.Test.E2E.TestPlans.NamiWallet as Nami
import HelloWorld.Test.E2E.TestPlans.KeyWallet as Key

import Node.Process (lookupEnv)

data TestWallet
  = Nami
  | Key

getTestWallet :: Effect TestWallet
getTestWallet =
  lookupEnv Env.testWallet >>= case _ of
    Just "KeyWallet" -> pure Key
    Just "NamiWallet" -> pure Nami
    _ -> throw "Unknow test wallet"

main ∷ Effect Unit
main = do
  noRuntime <- isJust <$> lookupEnv Env.noRuntime
  if noRuntime then
    log "skip the test since there's no ctl-runtime"
  else do
    helloWorldBrowserIndex <- lookupEnv Env.helloWorldBrowserIndex

    case helloWorldBrowserIndex of
      Nothing -> throw "HELLO_WORLD_BROWSER_INDEX not set"
      Just index ->
        launchAff_ do
          bracket (startStaticServer index) closeStaticServer $ \_ -> do
            liftEffect getTestWallet >>= case _ of
              Key -> Key.runTestPlans
              Nami -> Nami.runTestPlans

