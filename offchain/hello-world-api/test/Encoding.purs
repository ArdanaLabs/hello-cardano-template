module Test.HelloWorld.Encoding
  ( spec
  ) where

import Contract.Prelude

import CBOR as CBOR
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldNotEqual, shouldEqual)
import Util (decodeCbor)

spec :: Spec Unit
spec = describe "decodeCbor" $ do
  it "fails on invalid" $
    decodeCbor "invalid" `shouldEqual` Nothing
  it "hello decodes" $
    decodeCbor CBOR.hello `shouldNotEqual` Nothing

-- TODO shouldSatisify isJust would be better
-- as it wouldn't needlesly require (Eq a)
-- and imo is more readable
-- but shouldSatisify fails to import for some reason
