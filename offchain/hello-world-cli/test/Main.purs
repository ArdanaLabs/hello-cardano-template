module Test.Main
  ( main
  ) where

import Contract.Prelude
import Node.ChildProcess
  (Exit
  ,ChildProcess
  ,spawn
  ,defaultSpawnOptions
  ,onExit
  ,stdout
  ,stderr
  )
import Node.Stream(readString)
import Node.Encoding(Encoding(UTF8))
import Effect.Aff(makeAff,launchAff_)
import Data.Tuple.Nested(type (/\))

main :: Effect Unit
main = launchAff_ do
  res1 <- spawnAff "ls" ["bad_path"]
  res2 <- spawnAff "ls" ["-a"]
  log $ show $ res1
  log $ show $ res2
  pure unit

spawnAff :: String -> Array String -> Aff (Exit /\ String)
spawnAff cmd args = do
  (exit /\ child) <- spawnAff' cmd args
  out <- liftEffect $ readString (stdout child) Nothing UTF8
  err <- liftEffect $ readString (stderr child) Nothing UTF8
  pure $ (exit /\ fromMaybe "" (out <> err))

spawnAff' :: String -> Array String -> Aff (Exit /\ ChildProcess)
spawnAff' cmd args = makeAff (\callBack ->
  do
  child <- spawn cmd args defaultSpawnOptions
  onExit child (callBack <<< Right <<< (_ /\ child))
  pure mempty
  )
