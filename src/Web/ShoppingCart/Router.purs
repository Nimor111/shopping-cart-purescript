-- Thank you to the httpure-extras repos for the ideas on this
module Web.ShoppingCart.Router
  ( router
  , Route(..)
  , route
  , sayHello
  , errorOut
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.Foldable (find) as Foldable
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Retry (RetryStatus(..), recovering)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import HTTPure (Response, ok) as HTTPure
import HTTPure.Path (Path) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (notFound) as HTTPure
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Error (UnknownError, unknownError, type (+))
import Web.ShoppingCart.Retry (checks, retryPolicy)

data Route m
  = Route HTTPure.Path (HTTPure.Request -> m HTTPure.Response)

route ::
  forall m.
  HTTPure.Path ->
  (HTTPure.Request -> m HTTPure.Response) ->
  Route m
route = Route

router ::
  ∀ r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Array (Route m) ->
  HTTPure.Request ->
  m HTTPure.Response
router routes request@{ path } = do
  case Foldable.find (\(Route prefix _) -> startsWith prefix) routes of
    Just (Route prefix handler) -> do
      handler $ request { path = subpath prefix }
    Nothing -> HTTPure.notFound
  where
  startsWith :: HTTPure.Path -> Boolean
  startsWith prefix =
    Array.length path >= Array.length prefix
      && Array.take (Array.length prefix) path
      == prefix

  subpath :: HTTPure.Path -> HTTPure.Path
  subpath prefix = Array.drop (Array.length prefix) path

errorOut ::
  ∀ r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadError (Variant (UnknownError + r)) m =>
  HTTPure.Request ->
  m HTTPure.Response
errorOut _ = recovering retryPolicy checks action
  where
  action :: RetryStatus -> m HTTPure.Response
  action (RetryStatus { iterNumber: n }) = do
    liftEffect $ log ("Erroring out attempt: " <> show n)
    other <- asks _.other
    throwError $ unknownError $ error "this is an error"

sayHello ::
  ∀ r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  HTTPure.Request ->
  m HTTPure.Response
sayHello _ = do
  other <- asks _.other
  HTTPure.ok $ "Hello, " <> other
