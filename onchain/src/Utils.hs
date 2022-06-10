module Utils (validatorToHexString) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Numeric
import Data.Word (Word8)
import Plutus.V1.Ledger.Scripts (Validator)

validatorToHexString :: Validator -> String
validatorToHexString v = concatMap byteToHex $ BSL.unpack $ serialise v
  where
    byteToHex :: Word8 -> String
    byteToHex b = padToLen 2 '0' (showHex b "")
    padToLen :: Int -> Char -> String -> String
    padToLen len c w = replicate (len - length w) c <> w
