{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified Control.Concurrent as C

import Control.Monad
import Data.String
import Data.Text (unpack)

import Network.URI
import Test.QuickCheck
import Test.Syd
import Test.Syd.Webdriver
import Test.WebDriver
import System.Environment (getEnv)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp (run, Port)
import Network.Socket.Free

data Command = Init | Incr
  deriving (Show)

instance Arbitrary Command where
  arbitrary = elements [Init, Incr, Incr, Incr, Incr]

evalCommands :: [Command] -> Int
evalCommands = foldl f 0
  where
    f _ Init = 0
    f c Incr = c + 1

startHelloWorldUI :: Port -> IO ()
startHelloWorldUI port = do
  helloWorldUIIndex <- getEnv "HELLO_WORLD_UI_INDEX"
  run port $ staticApp (defaultFileServerSettings $ fromString helloWorldUIIndex)

initPage :: WebdriverSpec () -> Spec
initPage = webdriverSpec $ \_ -> do
  port <- liftIO getFreePort
  liftIO $ C.forkIO $ startHelloWorldUI port
  let uriStr = "http://127.0.0.1:" <> show port
  case parseURI uriStr of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse uri as string: " <> show uriStr
    Just uri -> pure (uri, ())

main :: IO ()
main = sydTest $
  initPage $
    it "test 1" $ \wte -> mapSize (* 10) $
      withMaxSuccess 5 $
        property $ \commands ->
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

            when (n /= (show $ evalCommands commands)) $ error "fail"
