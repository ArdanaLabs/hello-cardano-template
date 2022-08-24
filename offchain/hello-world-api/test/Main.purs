module Test.Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Effect.Exception (throw)
import Test.Encoding as Encoding
import Test.HelloWorld.Api as Test.HelloWorld.Api

import Node.Process (lookupEnv)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Data.Maybe (Maybe(Just, Nothing))

main :: Effect Unit
main = do
  usePlutip <- lookupEnv "MODE" >>= case _ of
    Just "local" -> pure true
    Just "testnet" -> pure false
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  launchAff_ $ runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $
    if usePlutip then pureSpecs
    else pure unit

pureSpecs :: Spec Unit
pureSpecs = do
  describe "pure tests" do
    Encoding.spec
    Test.HelloWorld.Api.spec
