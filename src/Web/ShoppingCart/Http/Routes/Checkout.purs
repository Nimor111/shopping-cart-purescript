module Web.ShoppingCart.Http.Routes.Checkout where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Logger.Class (class MonadLogger, info)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut (parseJson, printJsonDecodeError)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Either (Either(..))
import Data.Map.Internal (empty)
import Data.Newtype (wrap)
import Data.Traversable (sequence)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import HTTPure ((!@))
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, created, notFound) as HTTPure
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Card (Card)
import Web.ShoppingCart.Domain.Order (OrderId)
import Web.ShoppingCart.Domain.User (UserId)
import Web.ShoppingCart.Effects.Background (class Background)
import Web.ShoppingCart.Error (type (+), ShoppingCartJsonDecodeError, OrderCreateFailedError, PaymentFailedError, jsonDecodeError)
import Web.ShoppingCart.Programs.Checkout (checkout)
import Web.ShoppingCart.Services.Orders (Orders)
import Web.ShoppingCart.Services.Payments (Payments)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)

type HandleCheckoutError r
  = Variant (ShoppingCartJsonDecodeError + OrderCreateFailedError + PaymentFailedError + r)

checkoutRouter ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadError (HandleCheckoutError r) m =>
  Background m =>
  Payments m ->
  ShoppingCart m ->
  Orders m ->
  HTTPure.Request ->
  m HTTPure.Response
checkoutRouter payments cart orders req@{ path, method: Post, body } = do
  res <- handleCheckout payments cart orders (wrap $ path !@ 0) body
  case res of
    Left err -> throwError err
    Right v -> HTTPure.created

checkoutRouter _ _ _ _ = HTTPure.notFound

handleCheckout ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadError (HandleCheckoutError r) m =>
  Background m =>
  Payments m ->
  ShoppingCart m ->
  Orders m ->
  UserId ->
  String ->
  m (Either (HandleCheckoutError r) OrderId)
handleCheckout payments cart orders userId body =
  runExceptT
    $ do
        info empty $ "Checking out order for user " <> show userId
        card <- ExceptT $ pure $ mapJsonError body
        ExceptT $ sequence $ Right (checkout payments cart orders userId card)
  where
  mapJsonError :: forall r1. String -> Either (Variant (ShoppingCartJsonDecodeError + r1)) Card
  mapJsonError body = case decodeJson =<< parseJson body of
    Left error -> Left $ jsonDecodeError (printJsonDecodeError error)
    Right v -> Right v
