-- Thank you to the httpure-extras repos for the ideas on this
module Web.ShoppingCart.Router
  ( router
  , Route(..)
  , route
  , sayHello
  , insertPeople
  , errorOut
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.Foldable (find) as Foldable
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Retry (RetryStatus(..), recovering)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import HTTPure (Response, ok) as HTTPure
import HTTPure.Path (Path) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (notFound) as HTTPure
import Selda.PG.Class (insert_)
import Web.ShoppingCart.App (AppError, App)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Database (hoistSelda, people)
import Web.ShoppingCart.Error (UnknownError, unknownError, type (+))
import Web.ShoppingCart.Http.Routes.Brands (brandsRouter)
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
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Array (Route m) ->
  HTTPure.Request ->
  m HTTPure.Response
router routes request@{ path } = case Foldable.find (\(Route prefix _) -> startsWith prefix) routes of
  Just (Route prefix handler) -> handler $ request { path = subpath prefix }
  Nothing -> HTTPure.notFound
  where
  startsWith :: HTTPure.Path -> Boolean
  startsWith prefix =
    Array.length path >= Array.length prefix
      && Array.take (Array.length prefix) path
      == prefix

  subpath :: HTTPure.Path -> HTTPure.Path
  subpath prefix = Array.drop (Array.length prefix) path

insertPeople ::
  ∀ r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  HTTPure.Request ->
  m HTTPure.Response
insertPeople _ =
  hoistSelda (insert_ people peopleData)
    >>= (\_ -> HTTPure.ok $ "Inserted.")
  where
  peopleData :: Array { id :: Int, name :: String, age :: Maybe Int }
  peopleData =
    [ { id: 1, name: "name1", age: Just 11 }
    , { id: 2, name: "name2", age: Just 22 }
    , { id: 3, name: "name3", age: Just 33 }
    , { id: 4, name: "name4", age: Just 44 }
    , { id: 5, name: "name5", age: Just 55 }
    ]

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
