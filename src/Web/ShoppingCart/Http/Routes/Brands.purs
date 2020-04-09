module Web.ShoppingCart.Http.Routes.Brands
        ( brandsRouter
        ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, notFound, ok') as HTTPure
import Simple.JSON as JSON
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Brands (Brands)


brandsRouter
    :: forall m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => Brands m
    -> HTTPure.Request
    -> m HTTPure.Response
brandsRouter brands req@{ path: [""] } = getBrands brands req
brandsRouter _ _ = HTTPure.notFound

getBrands
    :: forall m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => Brands m
    -> HTTPure.Request
    -> m HTTPure.Response
getBrands b req = do
    liftEffect $ log "Fetching all brands..."
    brands <- b.findAll

    HTTPure.ok' responseHeaders (JSON.writeJSON brands)
