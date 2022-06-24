module Test.Encoding
  ( spec
  ) where


import Contract.Prelude

import CBOR as CBOR
import Test.Spec(Spec,it,describe)
import Test.Spec.Assertions(shouldNotEqual)
import Contract.Aeson (decodeAeson, fromString)
import Contract.Scripts (Validator)

spec :: Spec Unit
spec = do
  describe "hello" $ testCborDecodes CBOR.hello

testCborDecodes :: String -> Spec Unit
testCborDecodes cbor = it "decode test" $ decodeCbor cbor `shouldNotEqual` Nothing

decodeCbor :: String -> Maybe Validator
decodeCbor = fromString >>> decodeAeson >>> hush >>> map wrap
