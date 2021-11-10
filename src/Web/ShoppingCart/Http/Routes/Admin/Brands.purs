module Web.ShoppingCart.Http.Routes.Admin.Brands where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Traversable (sequence)
import Data.UUID (genUUID, toString)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, created, notFound) as HTTPure
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Brand (RefinedBrandDTO(..))
import Web.ShoppingCart.Domain.RefinedPred (nameToDomain)
import Web.ShoppingCart.Error (ShoppingCartJsonDecodeError, jsonDecodeError, type (+))
import Web.ShoppingCart.Services.Brands (Brands)

brandsRouter ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadThrow (Variant (ShoppingCartJsonDecodeError + r)) m =>
  Brands m ->
  HTTPure.Request ->
  m HTTPure.Response
brandsRouter brands { method: Post, path: [], body } = do
  res <- createBrand brands body
  case res of
    Left err -> throwError err
    Right _ -> HTTPure.created

brandsRouter _ _ = HTTPure.notFound

createBrand ::
  forall r m.
  MonadAff m =>
  MonadThrow (Variant (ShoppingCartJsonDecodeError + r)) m =>
  Brands m ->
  String ->
  m (Either (Variant (ShoppingCartJsonDecodeError r)) Unit)
createBrand brands body =
  runExceptT
    $ do
        (RefinedBrandDTO _ brandName) <- ExceptT $ pure $ mapJsonError body
        uuid <- ExceptT $ sequence $ Right $ liftEffect genUUID
        ExceptT $ sequence $ Right (brands.create $ { id: (wrap $ toString uuid), name: nameToDomain brandName })
  where
  mapJsonError :: String -> Either (Variant (ShoppingCartJsonDecodeError + r)) RefinedBrandDTO
  mapJsonError body = case (decodeJson =<< parseJson body) of
    -- TODO Use the JsonDecodeError from argonaut directly
    Left errors -> Left $ jsonDecodeError (printJsonDecodeError errors)
    Right v -> Right v
