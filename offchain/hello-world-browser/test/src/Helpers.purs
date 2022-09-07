module HelloWorld.Test.Helpers where

import Contract.Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeFromForeign)
import Foreign as Foreign
import HelloWorld.Test.Constants as Constants
import Node.Express.App (listenHttp, use)
import Node.Express.Handler (HandlerM)
import Node.Express.Middleware.Static (static)
import Node.Express.Response (clearCookie, setCookie)
import Node.Express.Types (defaultCookieOptions)
import Node.HTTP (close, Server)
import Toppokki as T

-- | A String representing a jQuery selector, e.g. "#my-id" or ".my-class"
newtype Selector = Selector String

derive instance Newtype Selector _
-- | A String representing a jQuery action, e.g. "click" or "enable".
newtype Action = Action String

derive instance Newtype Action _

helloWorldBrowserURL :: T.URL
helloWorldBrowserURL = T.URL $ Constants.localhost <> ":" <> show Constants.port

-- | Build a primitive jQuery expression like '$("button").click()' and
-- | out of a selector and action and evaluate it in Toppokki
doJQ :: Selector -> Action -> T.Page -> Aff Foreign
doJQ selector action page = do
  T.unsafeEvaluateStringFunction jq page
  where
  jq :: String
  jq = "$('" <> unwrap selector <> "')." <> unwrap action

-- | select a button with a specific text inside
buttonWithText :: String -> Selector
buttonWithText text = wrap $ "button:contains(" <> text <> ")"

click :: Action
click = wrap "click()"

clickButton :: String -> T.Page -> Aff Unit
clickButton buttonText = void <$> doJQ (buttonWithText buttonText) click

injectJQuery :: String -> T.Page -> Aff Unit
injectJQuery jQuery page = do
  (alreadyInjected :: Boolean) <-
    unsafeFromForeign <$>
      T.unsafeEvaluateStringFunction "typeof(jQuery) !== 'undefined'"
        page
  unless alreadyInjected $ void $ T.unsafeEvaluateStringFunction jQuery
    page

newtype Cookie = Cookie
  { key :: String
  , value :: Maybe String
  }

derive instance newtypeCookie :: Newtype Cookie _
derive instance eqCookie :: Eq Cookie

mkCookies :: Maybe String -> Maybe String -> Maybe String -> Array Cookie
mkCookies paymentKey stakeKey networkId =
  [ Cookie { key: "paymentKey", value: paymentKey }
  , Cookie { key: "stakeKey", value: stakeKey }
  , Cookie { key: "networkId", value: networkId }
  ]

startStaticServer :: Array Cookie -> String -> Aff Server
startStaticServer cookies directory = do
  liftEffect $ listenHttp (use staticServer) Constants.port $ \_ -> pure unit
  where
  staticServer :: HandlerM Unit
  staticServer = do
    for_ cookies $ \(Cookie { key, value }) ->
      case value of
        Just value' -> setCookie key value' defaultCookieOptions
        Nothing -> clearCookie key "/"
    static directory

closeStaticServer :: Server -> Aff Unit
closeStaticServer server = liftEffect $ close server (pure unit)

getCurrentValueHeader :: String
getCurrentValueHeader = "[].map.call(document.querySelectorAll('#current-value-header'), el => el.textContent)"

getCurrentValueBody :: String
getCurrentValueBody = "[].map.call(document.querySelectorAll('#current-value-body'), el => el.textContent)"

getFundsLockedHeader :: String
getFundsLockedHeader = "[].map.call(document.querySelectorAll('#funds-locked-header'), el => el.textContent)"

getFundsLockedBody :: String
getFundsLockedBody = "[].map.call(document.querySelectorAll('#funds-locked-body'), el => el.textContent)"

readString :: Foreign -> String
readString = Foreign.unsafeFromForeign
