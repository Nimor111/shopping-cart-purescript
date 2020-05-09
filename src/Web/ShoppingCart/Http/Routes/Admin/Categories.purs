module Web.ShoppingCart.Http.Routes.Admin.Categories where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Show (show)
import Data.Traversable (sequence)
import Data.UUID (genUUID, toString)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, created, notFound, ok') as HTTPure
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Category (Category, CategoryName(..), RefinedCategoryDTO(..))
import Web.ShoppingCart.Domain.RefinedPred (nameToDomain)
import Web.ShoppingCart.Error (JsonDecodeError, jsonDecodeError, type (+))
import Web.ShoppingCart.Services.Categories (Categories)

categoriesRouter ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  Categories m ->
  HTTPure.Request ->
  m HTTPure.Response
categoriesRouter categories req@{ method: Post, path: [], body } = do
  res <- createCategory categories body
  case res of
    Left err -> throwError err
    Right _ -> HTTPure.created

categoriesRouter _ _ = HTTPure.notFound

createCategory ::
  forall r m.
  MonadAff m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  Categories m ->
  String ->
  m (Either (Variant (JsonDecodeError r)) Unit)
createCategory categories body =
  runExceptT
    $ do
        (RefinedCategoryDTO _ categoryName) <- ExceptT $ pure $ mapJsonError body
        uuid <- ExceptT $ sequence $ Right $ liftEffect genUUID
        ExceptT $ sequence $ Right (categories.create $ { id: (wrap $ toString uuid), name: nameToDomain categoryName })
  where
  mapJsonError :: String -> Either (Variant (JsonDecodeError + r)) RefinedCategoryDTO
  mapJsonError body = case (decodeJson =<< jsonParser body) of
    Left errors -> Left $ jsonDecodeError errors
    Right v -> Right v
