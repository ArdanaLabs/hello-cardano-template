{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad
import Data.String
import Data.Text (unpack)
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.URI
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import System.Environment (getEnv)
import Test.QuickCheck (mapSize, withMaxSuccess)
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Wai
import Test.Syd.Webdriver
import Test.WebDriver

data Command = Init | Incr
  deriving (Show, Eq, Generic)

instance Validity Command -- Implementation is derived via Generic

instance GenValid Command

evalCommands :: [Command] -> Int
evalCommands = foldl f 0
  where
    f _ Init = 0
    f c Incr = c + 1

startHelloWorldBrowser :: SetupFunc URI
startHelloWorldBrowser = do
  helloWorldBrowserIndex <- liftIO $ getEnv "HELLO_WORLD_BROWSER_INDEX"
  port <- applicationSetupFunc $ staticApp (defaultFileServerSettings $ fromString helloWorldBrowserIndex)
  let uriStr = "http://127.0.0.1:" <> show port
  case parseURI uriStr of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse uri as string: " <> show uriStr
    Just uri -> pure uri

setupWebdriverTestEnv :: URI -> SetupFunc (WebdriverTestEnv ())
setupWebdriverTestEnv uri = do
  ssh <- seleniumServerSetupFunc
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  webdriverTestEnvSetupFunc ssh manager uri ()

main :: IO ()
main = sydTest $
  setupAround (startHelloWorldBrowser >>= setupWebdriverTestEnv) $
    it "test 1" $ \wte -> mapSize (* 10) $
      withMaxSuccess 5 $
        forAllValid $ \commands ->
          runWebdriverTestM wte $ do
            openPath ""
            initialize <- findElem $ ById "initialize"
            increment <- findElem $ ById "increment"
            counter <- findElem $ ById "counter"

            let interpret c = click $ case c of
                  Init -> initialize
                  Incr -> increment

            mapM_ interpret commands
            n <- unpack <$> getText counter

            when (n /= show (evalCommands commands)) $ error "fail"
