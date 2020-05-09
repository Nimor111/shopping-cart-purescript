module Web.ShoppingCart.Http.Routes.Orders where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Logger.Class (class MonadLogger, info)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Map.Internal (empty)
import Data.Newtype (un, unwrap, wrap)
import Effect.Aff.Class (class MonadAff)
import HTTPure ((!@), (!?))
import HTTPure.Body (class Body)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, noContent', notFound, ok, ok') as HTTPure
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Order (OrderId(..))
import Web.ShoppingCart.Domain.User (UserId(..))
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Orders (Orders)

ordersRouter ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Orders m ->
  HTTPure.Request ->
  m HTTPure.Response
ordersRouter orders req@{ path, method: Get } = case path !? 1 of
  true -> getOrder (wrap $ path !@ 0) (wrap $ path !@ 1) orders req
  false -> getOrdersByUser (wrap $ path !@ 0) orders req

ordersRouter _ _ = HTTPure.notFound

getOrder ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  UserId ->
  OrderId ->
  Orders m ->
  HTTPure.Request ->
  m HTTPure.Response
getOrder userId orderId orders req = do
  info empty $ "Fetching order with id " <> show orderId <> "for user " <> show userId
  order <- orders.get userId orderId
  HTTPure.ok' responseHeaders (stringify $ encodeJson order)

getOrdersByUser ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  UserId ->
  Orders m ->
  HTTPure.Request ->
  m HTTPure.Response
getOrdersByUser userId orders req = do
  info empty $ "Fetching orders for user " <> show userId
  userOrders <- orders.findBy userId
  HTTPure.ok' responseHeaders (stringify $ encodeJson userOrders)
