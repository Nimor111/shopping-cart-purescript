module Web.ShoppingCart.Http.Routes.Admin.Brands where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Show (show)
import Data.Traversable (sequence)
import Data.UUID (genUUID)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, created, notFound, ok') as HTTPure
import Simple.JSON as JSON
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Brand (BrandName(..), Brand)
import Web.ShoppingCart.Error (JsonDecodeError, jsonDecodeError, type (+))
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Brands (Brands)


brandsRouter
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow (Variant (JsonDecodeError + r)) m
    => Brands m
    -> HTTPure.Request
    -> m HTTPure.Response
brandsRouter brands req@{ method: Post, path: [""], body } = do
    res <- createBrand brands body

    case res of
        Left err -> throwError err
        Right _ -> HTTPure.created
brandsRouter _ _ = HTTPure.notFound

createBrand
    :: forall r m
    .  MonadAff m
    => MonadThrow (Variant (JsonDecodeError + r)) m
    => Brands m
    -> String
    -> m (Either (Variant (JsonDecodeError r)) Unit)
createBrand brands body = runExceptT $ do
    brandName <- ExceptT $ pure $ mapJsonError body
    uuid <- ExceptT $ sequence $ Right $ liftEffect genUUID

    ExceptT $ sequence $ Right (brands.create $ {brandId: (wrap $ show uuid), brandName: brandName})

    where
        mapJsonError :: forall r1. String -> Either (Variant (JsonDecodeError + r1)) BrandName
        mapJsonError body = case JSON.readJSON body of
            Left errors -> Left $ jsonDecodeError errors
            Right v -> Right v