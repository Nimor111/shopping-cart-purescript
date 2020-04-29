module Web.ShoppingCart.Http.Routes.Admin.Items where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, created, notFound, ok) as HTTPure
import Simple.JSON as JSON
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Item (CreateItem, UpdateItem)
import Web.ShoppingCart.Error (JsonDecodeError, jsonDecodeError, type (+))
import Web.ShoppingCart.Services.Items (Items)

itemsRouter ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  Items m ->
  HTTPure.Request ->
  m HTTPure.Response
itemsRouter items req@{ method: Post, path: [ "" ], body } = do
  res <- createItem items body
  case res of
    Left err -> throwError err
    Right _ -> HTTPure.created

itemsRouter items req@{ method: Put, path: [ "" ], body } = do
  res <- updateItem items body
  case res of
    Left err -> throwError err
    Right _ -> HTTPure.ok ""

itemsRouter _ _ = HTTPure.notFound

createItem ::
  forall r m.
  MonadAff m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  Items m ->
  String ->
  m (Either (Variant (JsonDecodeError r)) Unit)
createItem items body =
  runExceptT
    $ do
        newItem <- ExceptT $ pure $ mapJsonError body
        -- uuid <- ExceptT $ sequence $ Right $ liftEffect genUUID
        ExceptT $ sequence $ Right $ items.create newItem
  where
  mapJsonError :: String -> Either (Variant (JsonDecodeError + r)) CreateItem
  mapJsonError b = case JSON.readJSON b of
    Left errors -> Left $ jsonDecodeError errors
    Right v -> Right v

updateItem ::
  forall r m.
  MonadAff m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  Items m ->
  String ->
  m (Either (Variant (JsonDecodeError r)) Unit)
updateItem items body =
  runExceptT
    $ do
        updatedItem <- ExceptT $ pure $ mapJsonError body
        ExceptT $ sequence $ Right $ items.update updatedItem
  where
  mapJsonError :: String -> Either (Variant (JsonDecodeError + r)) UpdateItem
  mapJsonError b = case JSON.readJSON b of
    Left errors -> Left $ jsonDecodeError errors
    Right v -> Right v
