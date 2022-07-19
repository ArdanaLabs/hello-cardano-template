module CmdUtils
  (passes
  ,fails
  ,spawnAff
  ) where

import Contract.Prelude
import Node.ChildProcess
  (Exit(Normally)
  ,ChildProcess
  ,spawn
  ,defaultSpawnOptions
  ,onExit
  ,stdout
  ,stderr
  )
import Node.Stream(readString)
import Node.Encoding(Encoding(UTF8))
import Effect.Aff(makeAff)
import Data.Tuple.Nested(type (/\))
import Data.Array(uncons)
import Data.String.Utils(words)
import Effect.Exception(Error)
import Test.Spec.Assertions(shouldReturn)

passes :: String -> Aff Unit
passes cmd = shouldReturn (testCmd cmd) true

fails :: String -> Aff Unit
fails cmd = shouldReturn (testCmd cmd) false

testCmd :: String -> Aff Boolean
testCmd cmd = spawnAff' cmd <#> fst >>> isSuccess

isSuccess :: Exit -> Boolean
isSuccess (Normally 0) = true
isSuccess _ = false

spawnAff :: String -> Aff (Exit /\ String)
spawnAff cmd = do
  (exit /\ child) <- spawnAff' cmd
  out <- liftEffect $ readString (stdout child) Nothing UTF8
  err <- liftEffect $ readString (stderr child) Nothing UTF8
  pure $ (exit /\ fromMaybe "" (out <> err))

spawnAff' :: String -> Aff (Exit /\ ChildProcess)
spawnAff' cmd = makeAff (\callBack ->
  do
  let (exec /\ args) = parseCmd cmd
  child <- spawn exec args defaultSpawnOptions
  onExit child (callBack <<< Right <<< (_ /\ child))
  pure mempty
  )

parseCmd :: String -> (String /\ Array String)
parseCmd str = case uncons (words str) of
  Just {head,tail} -> (head /\ tail)
  Nothing -> ("" /\ [])

