module HelloWorld.Page.Home where

import Prelude

import Affjax.RequestBody as AB
import Affjax.ResponseFormat as AR
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AW
import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import HelloWorld.Action.Contract as Contract
import HelloWorld.ContractID (ContractID(..))
import HelloWorld.Endpoint as E
import HelloWorld.Env (Env)
import Network.RemoteData (RemoteData(..), fromEither)

data Action
  = Initialize
  | Contract Contract.Action

type State =
  { activate :: RemoteData String ContractID
  , initialize :: RemoteData String Unit
  , increment :: RemoteData String Unit
  , read :: RemoteData String Unit
  }

isSuccessfulResponse :: forall a. AW.Response a -> Boolean
isSuccessfulResponse resp = status >= 200 && status < 400
  where
  (StatusCode status) = resp.status

_activate
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => m (Either String ContractID)
_activate = do
  { baseURL, wallet } <- ask
  response <-
    liftAff
      $ AW.request
          ( AW.defaultRequest
              { url = E.toURL baseURL E.Activate
              , method = Left POST
              , content = Just $ AB.json $ A.fromObject
                  ( Object.fromFoldable
                      [ Tuple "caID"
                          ( A.fromObject
                              ( Object.fromFoldable
                                  [ Tuple "tag" (A.fromString "Initialize") ]
                              )
                          )
                      , Tuple "caWallet"
                          ( A.fromObject
                              ( Object.fromFoldable
                                  [ Tuple "getWalletId" (A.fromString (show wallet)) ]
                              )
                          )
                      ]
                  )
              , responseFormat = AR.json
              }
          )
  case response of
    Left err -> pure $ Left $ AW.printError err
    Right res -> do
      case (decodeJson (res.body) :: Either JsonDecodeError { unContractInstanceId :: String }) of
        Left decodeError -> pure $ Left (show decodeError)
        Right contract -> pure $ Right (ContractID contract.unContractInstanceId)

_initialize
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ContractID
  -> m (Either String Unit)
_initialize contractID = do
  { baseURL } <- ask
  response <-
    liftAff
      $ AW.request
          ( AW.defaultRequest
              { url = E.toURL baseURL (E.Initialize contractID)
              , method = Left POST
              , content = Just $ AB.json $ A.fromArray []
              , responseFormat = AR.json
              }
          )
  case response of
    Left err -> pure $ Left $ AW.printError err
    Right response'
      | isSuccessfulResponse response' -> pure $ Right unit
      | otherwise -> pure $ Left response'.statusText

_increment
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ContractID
  -> m (Either String Unit)
_increment contractID = do
  { baseURL } <- ask
  response <-
    liftAff
      $ AW.request
          ( AW.defaultRequest
              { url = E.toURL baseURL (E.Increment contractID)
              , method = Left POST
              , content = Just $ AB.json $ A.fromArray []
              , responseFormat = AR.json
              }
          )
  case response of
    Left err -> pure $ Left $ AW.printError err
    Right response'
      | isSuccessfulResponse response' -> pure $ Right unit
      | otherwise -> pure $ Left response'.statusText

_read
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ContractID
  -> m (Either String Unit)
_read contractID = do
  { baseURL } <- ask
  response <-
    liftAff
      $ AW.request
          ( AW.defaultRequest
              { url = E.toURL baseURL (E.Read contractID)
              , method = Left POST
              , content = Just $ AB.json $ A.fromArray []
              , responseFormat = AR.json
              }
          )
  case response of
    Left err -> pure $ Left $ AW.printError err
    Right response'
      | isSuccessfulResponse response' -> pure $ Right unit
      | otherwise -> pure $ Left response'.statusText

component
  :: forall q o m
   . MonadAff m
  => MonadAsk Env m
  => H.Component q Unit o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  initialState :: State
  initialState =
    { activate: NotAsked
    , initialize: NotAsked
    , increment: NotAsked
    , read: NotAsked
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction (Contract Contract.Activate)
    Contract Contract.Activate -> do
      H.modify_ _ { activate = Loading }
      result <- _activate
      H.modify_ _ { activate = fromEither result }
    Contract (Contract.Initialize contractID) -> do
      H.modify_ _ { initialize = Loading }
      result <- _initialize contractID
      H.modify_ _ { initialize = fromEither result }
    Contract (Contract.Increment contractID) -> do
      H.modify_ _ { increment = Loading }
      result <- _increment contractID
      H.modify_ _ { increment = fromEither result }
    Contract (Contract.Read contractID) -> do
      H.modify_ _ { read = Loading }
      result <- _read contractID
      H.modify_ _ { read = fromEither result }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { activate, initialize, increment, read } =
    HH.main_
      case activate of
        NotAsked -> [ HH.text "" ]
        Loading -> [ HH.text "Activating …" ]
        Failure err -> [ HH.text $ "Failed to activate: " <> err ]
        Success contractID ->
          [ HH.div_
              [ case initialize of
                  NotAsked -> HH.text ""
                  Loading -> HH.text "Initializing …"
                  Success _ -> HH.text "Initialize succeeded"
                  Failure err -> HH.text $ "Failed to initialize: " <> err
              ]
          , HH.div_
              [ case increment of
                  NotAsked -> HH.text ""
                  Loading -> HH.text "Incrementing …"
                  Success _ -> HH.text "Increment succeeded"
                  Failure err -> HH.text $ "Failed to increment: " <> err
              ]
          , HH.div_
              [ case read of
                  NotAsked -> HH.text ""
                  Loading -> HH.text "Reading …"
                  Success _ -> HH.text "Read succeeded"
                  Failure err -> HH.text $ "Failed to read: " <> err
              ]
          , HH.button
              [ HE.onClick \_ -> Contract (Contract.Initialize contractID)
              ]
              [ HH.text "Initialize"
              ]
          , HH.button
              [ HE.onClick \_ -> Contract (Contract.Increment contractID)
              ]
              [ HH.text "Increment"
              ]
          , HH.button
              [ HE.onClick \_ -> Contract (Contract.Read contractID)
              ]
              [ HH.text "Read"
              ]
          ]
