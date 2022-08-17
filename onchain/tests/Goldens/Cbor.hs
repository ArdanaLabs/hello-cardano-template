module Goldens.Cbor (spec) where

import Test.Syd
import Hello(trivialCbor,helloWorldCbor,paramHelloCbor)

spec :: Spec
spec = describe "goldens" $ do
  it "trivial" $
      pureGoldenStringFile "./goldens/trivial" (show trivialCbor)
  it "hello" $
      pureGoldenStringFile "./goldens/hello" helloWorldCbor
  it "param-hello" $
      pureGoldenStringFile "./goldens/param-hello" (show paramHelloCbor)


