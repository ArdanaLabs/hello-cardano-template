module Main where

import Network.URI
import Test.Syd
import Test.Syd.Wai
import Test.Syd.Webdriver

import Network.HTTP.Types as HTTP
import Network.Wai as Wai

main :: IO ()
main = sydTest $ webdriverSpec (\_ -> initPage) $ do
  it "test 1" $
    openPath "/"

initPage = do
  portNumber <- applicationSetupFunc exampleApplication
  let uriStr = "http://127.0.0.1:" <> show portNumber
  case parseURI uriStr of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse uri as string: " <> show uriStr
    Just uri -> pure (uri, ())

exampleApplication :: Wai.Application
exampleApplication req sendResp = do
  lb <- strictRequestBody req
  sendResp $ responseLBS HTTP.ok200 (Wai.requestHeaders req) lb
