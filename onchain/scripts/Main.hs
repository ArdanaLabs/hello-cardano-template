module Main (main) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (chr)
import Data.Word (Word8)
import Hello (helloValidator)
import Plutus.V1.Ledger.Scripts (getValidator)
import System.Environment (getArgs)

main :: IO ()
main = do
  [out] <- getArgs
  let bsl = serialise $ getValidator helloValidator
  writeFile (out ++ "/hello_world.plc") (concatMap byteToHex $ BSL.unpack bsl)

byteToHex :: Word8 -> String
byteToHex b =
  let h1 = b `div` 16
      h2 = b `mod` 16
   in [toChr h1, toChr h2]

toChr :: Word8 -> Char
toChr w
  | w < 10 = chr $ fromIntegral $ w + 48
  | otherwise = chr $ fromIntegral $ w + 87
