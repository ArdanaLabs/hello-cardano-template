module CmdUtils
  (passes
  ,fails
  ,failsSaying
  ,passesSaying
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
import Test.Spec.Assertions(fail)
import Data.String(Pattern(Pattern),contains)

passes :: String -> Aff Unit
passes cmd = do
  (exit /\ _) <- spawnAff' cmd
  unless (exit # isSuccess) $
    fail $ "unexpected failure running: " <> cmd
            <> "\nexited with: " <> show exit

passesSaying :: String -> String -> Aff Unit
passesSaying cmd msg = do
  (exit /\ output) <- spawnAff cmd
  unless (exit # isSuccess) $
    fail $ "unexpected failure running: " <> cmd
            <> "\nexited with: " <> show exit
  unless (contains (Pattern msg) output) $
    fail $
      "output did not match running:" <> cmd <>
      "\nexpected to contain: " <> msg <>
      "\noutput was:\n" <> output

fails :: String -> Aff Unit
fails cmd = do
  (exit /\ _) <- spawnAff' cmd
  when (exit # isSuccess) $
    fail $ "unexpected succes running: " <> cmd

failsSaying :: String -> String -> Aff Unit
failsSaying cmd errMsg = do
  (exit /\ output) <- spawnAff cmd
  when (exit # isSuccess) $
    fail $ "unexpected succes running: " <> cmd
  unless (contains (Pattern errMsg) output) $
    fail $
      "errMsg did not match running:" <> cmd <>
      "\nexpected to contain: " <> errMsg <>
      "\noutput was:\n" <> output

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

