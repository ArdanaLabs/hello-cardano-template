{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Main where

import Data.Text (unpack)
import Control.Monad

import Network.URI
import Test.Syd
import Test.Syd.Webdriver
import Test.WebDriver
import Test.QuickCheck
import Paths_selenium_example

data Command = Init | Inc | Read
  deriving Show

instance Arbitrary Command where
  arbitrary = elements [Init, Inc, Inc, Inc, Inc, Read, Read]

evalCommands :: [Command] -> String
evalCommands = snd . foldl (flip f) (0, "")  where

  f Init (_, c) = (0, c)
  f Inc (i, c) = (i + 1, c)
  f read (i, _) = (i, show i)

initPage = do
  path <- liftIO getDataDir
  let uriStr = "file://" <> path <> "/HelloWorld.html"
  case parseURI uriStr of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse uri as string: " <> show uriStr
    Just uri -> pure (uri, ())

main :: IO ()
main = sydTest $ webdriverSpec (\_ -> initPage) $
  it "test 1" $ \wte -> property $ \commands ->
    runWebdriverTestM wte $ do

      initialize <- findElem $ ById "initialize"
      increment  <- findElem $ ById "increment"
      read       <- findElem $ ById "read"
      counter    <- findElem $ ById "counterr"

      let interpret c = click $ case c of
           Init -> initialize
           Inc -> increment
           Read -> read

      mapM_ interpret commands
      n <- unpack <$> getText counter

      when (n /= evalCommands commands) $ error "fail"

