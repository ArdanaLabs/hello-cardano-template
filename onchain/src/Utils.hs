module Utils
  (validatorToHexString
  , trivialHexString
  , closedTermToHexString
  , CBOR(..)
  , toPs
  ) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.Word (Word8)
import Data.List (intercalate)
import Numeric
import Plutarch (compile)
import Plutarch.Api.V1
import Plutarch.Prelude
import Plutus.V1.Ledger.Scripts (Validator)

{- | This function turns a validator into a hex string usable with CTL.
 It works by serialising  the validator to a cbor byte string,
 unpacking the bytestring into a [Word8],
 and then using showHex from numeric (with a helper to pad 0s as needed)
 to show each byte in hexidecimal.
-}
validatorToHexString :: Validator -> String
validatorToHexString v = concatMap byteToHex $ BSL.unpack $ serialise v

closedTermToHexString :: forall (p :: PType). ClosedTerm p -> String
closedTermToHexString t = concatMap byteToHex $ BSL.unpack $ serialise $ compile t

byteToHex :: Word8 -> String
byteToHex b = padToLen 2 '0' (showHex b "")

padToLen :: Int -> Char -> String -> String
padToLen len c w = replicate (len - length w) c <> w

trivialHexString :: String
trivialHexString = validatorToHexString $ mkValidator trivialValidator

trivialValidator :: ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque)
trivialValidator = plam $ \_ _ _ -> popaque $ pcon PUnit

data CBOR = CBOR{name :: String,cbor :: String}

toPs :: [CBOR] -> String
toPs cs =
  "module CBOR (\n  " <> intercalate ",\n  " (name <$> cs) <> "\n) where\n\n"
  <> intercalate "\n\n" (toDec <$> cs)

toDec :: CBOR -> String
toDec c = name c <> " :: String\n" <> name c <> " = \""
  <> cbor c <> "\""
