module Web.ShoppingCart.Http.Routes.Items where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Newtype (wrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure ((!@))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, notFound, ok') as HTTPure
import Simple.JSON as JSON
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Brand (BrandName(..))
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Items (Items)


itemsRouter
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow (AppError r) m
    => Items m
    -> HTTPure.Request
    -> m HTTPure.Response
itemsRouter items req@{ path: [""] } = getItemsByBrandName items req
itemsRouter _ _ = HTTPure.notFound

getItemsByBrandName
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow (AppError r) m
    => Items m
    -> HTTPure.Request
    -> m HTTPure.Response
getItemsByBrandName i req = do
    liftEffect $ log "Fetching all items by brand name..."
    items <- i.findBy (wrap (req.query !@ "brand"))

    HTTPure.ok' responseHeaders (JSON.writeJSON items)
