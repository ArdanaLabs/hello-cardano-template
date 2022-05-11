module HelloWorld.Page.Home where

import Prelude

import Affjax.RequestBody as AB
import Affjax.ResponseFormat as AR
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AW
import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Data.Argonaut.Core as A
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import HelloWorld.Action.Contract as Contract
import HelloWorld.Endpoint as E
import HelloWorld.Env (BaseURL(..), ContractID(..), Env)
import Network.RemoteData (RemoteData(..), fromEither)

data Action = Contract Contract.Action

type State =
  { initialize :: RemoteData String Unit
  , increment :: RemoteData String Unit
  , read :: RemoteData String Unit
  }

_req
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => E.Endpoint
  -> m (Either String Unit)
_req endpoint = do
  { baseURL, contractID } <- ask
  response <-
    liftAff
      $ AW.request
          ( AW.defaultRequest
              { url = requestURL baseURL contractID
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
  where
  requestURL (BaseURL baseURL) (ContractID contractID) =
    baseURL <> "/api/contract/instance/" <> contractID <> "/endpoint/"
      <> case endpoint of
        E.Initialize -> "initialize"
        E.Increment -> "increment"
        E.Read -> "read"
  isSuccessfulResponse resp = status >= 200 && status < 400
    where
    (StatusCode status) = resp.status

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
              }
    }
  where
  initialState :: State
  initialState =
    { initialize: NotAsked
    , increment: NotAsked
    , read: NotAsked
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Contract Contract.Initialize -> do
      H.modify_ _ { initialize = Loading }
      result <- _req E.Initialize
      H.modify_ _ { initialize = fromEither result }
    Contract Contract.Increment -> do
      H.modify_ _ { increment = Loading }
      result <- _req E.Increment
      H.modify_ _ { increment = fromEither result }
    Contract Contract.Read -> do
      H.modify_ _ { read = Loading }
      result <- _req E.Read
      H.modify_ _ { read = fromEither result }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { initialize, increment, read } =
    HH.main_
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
          [ HE.onClick \_ -> Contract Contract.Initialize
          ]
          [ HH.text "Initialize"
          ]
      , HH.button
          [ HE.onClick \_ -> Contract Contract.Increment
          ]
          [ HH.text "Increment"
          ]
      , HH.button
          [ HE.onClick \_ -> Contract Contract.Read
          ]
          [ HH.text "Read"
          ]
      ]
