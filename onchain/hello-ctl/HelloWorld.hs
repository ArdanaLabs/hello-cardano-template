module Main (main) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (chr)
import Data.Word (Word8)
import Hello (helloValidator)
import Plutus.V1.Ledger.Scripts (Validator)
import System.Environment (getArgs)

main :: IO ()
main = do
  [out] <- getArgs
  writeFile (out ++ "/hello_world.cbor") hellWorldHexString

hellWorldHexString :: String
hellWorldHexString = validatorToHexString helloValidator

validatorToHexString :: Validator -> String
validatorToHexString v = concatMap byteToHex $ BSL.unpack $ serialise v
  where
    byteToHex :: Word8 -> String
    byteToHex b =
      let h1 = b `div` 16
          h2 = b `mod` 16
       in [toChr h1, toChr h2]

    toChr :: Word8 -> Char
    toChr w
      | w < 10 = chr $ fromIntegral $ w + 48
      | otherwise = chr $ fromIntegral $ w + 87
