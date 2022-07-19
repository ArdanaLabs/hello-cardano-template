module Test.Main
  ( main
  ) where

import Contract.Prelude
import Node.ChildProcess(Exit,spawn,defaultSpawnOptions,onExit)
import Effect.Aff(makeAff,launchAff_)

main :: Effect Unit
main = launchAff_ do
  res1 <- spawnAff "ls" ["bad_path"]
  res2 <- spawnAff "ls" ["-a"]
  log $ show res1
  log $ show res2
  pure unit

spawnAff :: String -> Array String -> Aff Exit
spawnAff cmd args = makeAff (\callBack ->
  do
  child <- spawn cmd args defaultSpawnOptions
  onExit child (callBack <<< Right)
  pure mempty
  )
