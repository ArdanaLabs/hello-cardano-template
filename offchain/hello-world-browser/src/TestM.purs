module HelloWorld.TestM where

import Contract.Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import HelloWorld.Capability.HelloWorldApi (class HelloWorldApi, FundsLocked(..))

newtype TestM a = TestM (ReaderT Unit Aff a)

runTestM :: TestM ~> Aff
runTestM (TestM m) =
  runReaderT m unit

derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM
derive newtype instance monadEffectTestM :: MonadEffect TestM
derive newtype instance monadAffTestM :: MonadAff TestM

instance helloWorldApiTestM :: HelloWorldApi TestM where
  lock _ _ = do
    liftAff $ delay (Milliseconds 1000.0)
    pure $ Right (FundsLocked 6.0)

  increment _ _ = do
    liftAff $ delay (Milliseconds 1000.0)
    pure $ Right unit

  redeem _ = do
    liftAff $ delay (Milliseconds 1000.0)
    pure $ Right unit