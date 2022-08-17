module Goldens.Cbor (spec) where

import Data.List (intercalate)
import Data.Text (unpack)
import Hello (helloWorldCbor, paramHelloCbor, trivialCbor)
import Test.Syd

spec :: Spec
spec = describe "cbor" $ do
  defaultGolden "trivial" trivialCbor
  defaultGolden "hello" helloWorldCbor
  defaultGolden "param-hello" paramHelloCbor

-- If we add more goldens this should probably be moved somewhere more central ie. a new Goldens.hs
defaultGolden :: Show a => String -> a -> _
defaultGolden name a = do
  path <- reverse <$> getTestDescriptionPath
  let path' = "./" <> intercalate "/" (unpack <$> path) <> "/" <> name
  it name $ pureGoldenStringFile path' (show a)
