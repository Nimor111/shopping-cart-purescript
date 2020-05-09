module Web.ShoppingCart.Http.Routes.Brands
  ( brandsRouter
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Logger.Class (class MonadLogger, debug, info)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Map.Internal (empty)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, notFound, ok') as HTTPure
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Brand (Brand)
import Web.ShoppingCart.Http.Routes.Admin.Brands (createBrand)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Brands (Brands)

brandsRouter ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Brands m ->
  HTTPure.Request ->
  m HTTPure.Response
brandsRouter brands req@{ path: [], method: Get } = getBrands brands req

brandsRouter _ _ = HTTPure.notFound

getBrands ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Brands m ->
  HTTPure.Request ->
  m HTTPure.Response
getBrands b req = do
  info empty "Fetching all brands"
  brands <- b.findAll
  HTTPure.ok' responseHeaders (stringify $ encodeJson brands)
