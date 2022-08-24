module CmdUtils
  ( passes
  , fails
  , failsSaying
  , passesSaying
  , spawnAff
  ) where

import Contract.Prelude
import Node.ChildProcess
  ( ExecResult
  , execFile
  , defaultExecOptions
  )
import Node.Buffer.Class (toString)
import Node.Encoding (Encoding(UTF8))
import Effect.Aff (makeAff)
import Data.Tuple.Nested (type (/\))
import Data.Array (uncons)
import Data.String.Utils (words)
import Test.Spec.Assertions (fail)
import Data.String (Pattern(Pattern), contains)
import Effect.Exception (Error)

passes :: String -> Aff Unit
passes cmd = do
  (exit /\ out) <- spawnAff cmd
  case exit of
    Just err ->
      fail
        $ "unexpected failure running: "
        <> cmd
        <> "\nerred with: "
        <> show err
        <> "\noutput was:\n:"
        <> out
    Nothing -> pure unit

passesSaying :: String -> String -> Aff Unit
passesSaying cmd msg = do
  (exit /\ output) <- spawnAff cmd
  case exit of
    Just err ->
      fail
        $ "unexpected failure running: "
        <> cmd
        <> "\nexited with: "
        <> show err
        <> "\noutput was:\n:"
        <> output
    Nothing ->
      unless (contains (Pattern msg) output)
        $ fail
        $ "output did not match running:"
        <> cmd
        <> "\nexpected to contain: "
        <> msg
        <> "\noutput was:\n"
        <> output

fails :: String -> Aff Unit
fails cmd = do
  (exit /\ output) <- spawnAff cmd
  case exit of
    Nothing ->
      fail
        $ "unexpected succes running: "
        <> cmd
        <> "\noutput was:\n"
        <> output
    Just _ -> pure unit

failsSaying :: String -> String -> Aff Unit
failsSaying cmd errMsg = do
  (exit /\ output) <- spawnAff cmd
  case exit of
    Nothing ->
      fail
        $ "unexpected succes running: "
        <> cmd
        <> "\noutput was:\n"
        <> output
    Just _ ->
      unless (contains (Pattern errMsg) output)
        $ fail
        $ "errMsg did not match running:"
        <> cmd
        <> "\nexpected to contain: "
        <> errMsg
        <> "\noutput was:\n"
        <> output

spawnAff :: String -> Aff (Maybe Error /\ String)
spawnAff cmd = do
  let (file /\ args) = parseCmd cmd
  (res :: ExecResult) <- execAff file args
  out <- liftEffect $ toString UTF8 res.stdout
  err <- liftEffect $ toString UTF8 res.stderr
  pure $ (res.error /\ (out <> err))

execAff :: String -> Array String -> Aff ExecResult
execAff file args = makeAff $ \callBack -> do
  _child <- execFile file args defaultExecOptions (callBack <<< Right)
  pure mempty

parseCmd :: String -> (String /\ Array String)
parseCmd str = case uncons (words str) of
  Just { head, tail } -> (head /\ tail)
  Nothing -> ("" /\ [])

