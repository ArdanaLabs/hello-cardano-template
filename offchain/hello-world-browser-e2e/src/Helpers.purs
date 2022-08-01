module HelloWorld.Test.E2E.Helpers where

import Prelude

import Contract.Test.E2E (RunningExample, TestOptions(..), WalletExt, namiSign, withBrowser, withExample)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeFromForeign)
import Mote (test)
import Node.Express.App (listenHttp, use)
import Node.Express.Middleware.Static (static)
import Node.Express.Types (Port)
import Node.HTTP (close, Server)
import Node.Process (lookupEnv)
import TestM (TestPlanM)
import Toppokki as T

-- | A String representing a jQuery selector, e.g. "#my-id" or ".my-class"
newtype Selector = Selector String

derive instance Newtype Selector _
-- | A String representing a jQuery action, e.g. "click" or "enable".
newtype Action = Action String

derive instance Newtype Action _

port :: Port
port = 8080

helloWorldBrowserURL :: T.URL
helloWorldBrowserURL = T.URL $ "http://127.0.0.1:" <> show port

-- | Run an E2E test. Parameters are:
-- |   String: Just a name for the logs
-- |   Toppokki.URL: URL where the example is running
-- |   TestOptions: Options to start the browser with
-- |   WalletExt: An extension which should be used
-- |   RunningExample -> Aff a: A function which runs the test
runE2ETest
  :: forall (a :: Type)
   . String
  -> TestOptions
  -> WalletExt
  -> (RunningExample -> Aff a)
  -> TestPlanM Unit
runE2ETest example opts ext f = test example $ withBrowser opts ext $
  \browser -> withExample helloWorldBrowserURL browser $ void <<< f

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

namiSign' :: RunningExample -> Aff Unit
namiSign' = namiSign (wrap "ctlctlctl")

startStaticServer :: String -> Aff Server
startStaticServer directory =
  liftEffect $ listenHttp (use $ static directory) port $ \_ -> pure unit

closeStaticServer :: Server -> Aff Unit
closeStaticServer server = liftEffect $ close server (pure unit)

mkTestOptions :: Effect TestOptions
mkTestOptions = do
  chromeExe <- lookupEnv "CHROME_EXE"
  pure $ TestOptions
    { chromeExe
    , namiDir: "./test-data/nami"
    , geroDir: "./test-data/gero"
    , chromeUserDataDir: "./test-data/chrome-user-data"
    , noHeadless: true
    }