module Web.ShoppingCart.Http.Routes.Categories
  ( categoriesRouter
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Logger.Class (class MonadLogger, info)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Map.Internal (empty)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, notFound, ok') as HTTPure
import Simple.JSON as JSON
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Categories (Categories)

categoriesRouter ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Categories m ->
  HTTPure.Request ->
  m HTTPure.Response
categoriesRouter categories req@{ path: [], method: Get } = getCategories categories req

categoriesRouter _ _ = HTTPure.notFound

getCategories ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Categories m ->
  HTTPure.Request ->
  m HTTPure.Response
getCategories c req = do
  info empty $ "Fetching all categories"
  categories <- c.findAll
  HTTPure.ok' responseHeaders (JSON.writeJSON categories)
