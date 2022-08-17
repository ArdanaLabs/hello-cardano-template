module Goldens.Cbor (spec) where

import Test.Syd
import Hello(trivialCBOR,helloWorldHexString,paramHelloCBOR)

spec :: Spec
spec = describe "goldens" $ do
  it "trivial" $
      pureGoldenStringFile "./goldens/trivial" (show trivialCBOR)
  it "hello" $
      pureGoldenStringFile "./goldens/hello" helloWorldHexString
  it "param-hello" $
      pureGoldenStringFile "./goldens/param-hello" (show paramHelloCBOR)


