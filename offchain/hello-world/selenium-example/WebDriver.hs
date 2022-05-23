{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Main where

import Test.WebDriver
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (unpack)
import Test.QuickCheck
import Test.QuickCheck.Monadic
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

chromeConfig = useBrowser chrome defaultConfig

prop :: [Command] -> Property
prop commands = monadicIO $ do

  path <- liftIO getDataDir
  n <- run $ runSession chromeConfig . finallyClose $ do

    openPage $ "file://" <> path <> "/HelloWorld.html"

    initialize <- findElem $ ById "initialize"
    increment  <- findElem $ ById "increment"
    read       <- findElem $ ById "read"
    counter    <- findElem $ ById "counterr"

    let interpret c = click $ case c of
         Init -> initialize
         Inc -> increment
         Read -> read

    mapM_ interpret commands
    unpack <$> getText counter

  assert $ n == evalCommands commands

main :: IO ()
main = quickCheck $ mapSize (*10) $ withMaxSuccess 5 prop


