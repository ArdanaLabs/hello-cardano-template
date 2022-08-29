module Goldens.Cbor (spec) where

import Data.List (intercalate)
import Data.Text (unpack)
import Hello (helloWorldCbor, paramHelloCbor, trivialCbor)
import Test.Syd

spec :: FilePath -> Spec
spec dir =
  modifyRunSettings (\tde -> tde {testRunSettingGoldenStart = testRunSettingGoldenReset tde}) $
    describe "cbor" $ do
      defaultGolden dir "trivial" trivialCbor
      defaultGolden dir "hello" helloWorldCbor
      defaultGolden dir "param-hello" paramHelloCbor

-- If we add more goldens this should probably be moved somewhere more central ie. a new Goldens.hs
defaultGolden :: Show a => FilePath -> String -> a -> _
defaultGolden dir name a = do
  path <- tail . reverse <$> getTestDescriptionPath
  let path' = dir <> intercalate "/" (unpack <$> path) <> "/" <> name
  it name $ pureGoldenStringFile path' (show a)
