module Web.ShoppingCart.Http.Routes.Admin.Items where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Traversable (sequence)
import Data.UUID (genUUID)
import Data.UUID (toString)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, created, notFound, ok) as HTTPure
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Item (CreateItem, RefinedItemDTO(..), RefinedItemUpdateDTO(..), UpdateItem)
import Web.ShoppingCart.Domain.RefinedPred (nameToDomain, numToDomain, uuidToDomain)
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
itemsRouter items req@{ method: Post, path: [], body } = do
  res <- createItem items body
  case res of
    Left err -> throwError err
    Right _ -> HTTPure.created

itemsRouter items req@{ method: Put, path: [], body } = do
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
        (RefinedItemDTO _ itemName itemDescription itemPrice itemBrand itemCategory) <- ExceptT $ pure $ mapJsonError body
        uuid <- ExceptT $ sequence $ Right $ liftEffect genUUID
        ExceptT $ sequence $ Right $ items.create { id: (wrap $ toString uuid), name: nameToDomain itemName, description: nameToDomain itemDescription, price: numToDomain itemPrice, brandId: nameToDomain itemBrand, categoryId: nameToDomain itemCategory }
  where
  mapJsonError :: String -> Either (Variant (JsonDecodeError + r)) RefinedItemDTO
  mapJsonError b = case decodeJson =<< jsonParser b of
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
        (RefinedItemUpdateDTO itemId itemPrice) <- ExceptT $ pure $ mapJsonError body
        ExceptT $ sequence $ Right $ items.update { id: uuidToDomain itemId, price: numToDomain itemPrice }
  where
  mapJsonError :: String -> Either (Variant (JsonDecodeError + r)) RefinedItemUpdateDTO
  mapJsonError b = case decodeJson =<< jsonParser b of
    Left errors -> Left $ jsonDecodeError errors
    Right v -> Right v
