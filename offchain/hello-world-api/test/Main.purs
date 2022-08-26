module Test.Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.Encoding as Encoding
import Test.HelloWorld.Api as Test.HelloWorld.Api
import Test.Spec (Spec, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec', defaultConfig)

main :: Effect Unit
main = do
  usePlutip <- lookupEnv "MODE" >>= case _ of
    Just "local" -> pure true
    Just "testnet" -> pure false
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  launchAff_ $ runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $
    if usePlutip then pureSpec
    else impureSpec

pureSpec :: Spec Unit
pureSpec = do
  describe "pure tests" do
    Encoding.spec
    Test.HelloWorld.Api.spec

impureSpec :: Spec Unit
impureSpec = do
  describe "impure tests" do
    Test.HelloWorld.Api.testnetSpec
