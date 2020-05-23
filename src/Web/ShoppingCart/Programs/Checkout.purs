module Web.ShoppingCart.Programs.Checkout
  ( checkout
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, try, catchError, throwError)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Data.UUID (genUUID, toString)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Retry (RetryStatus, recovering)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Exception (Error)
import Effect.Exception (error)
import Test.Unit.Console (log)
import Web.ShoppingCart.Domain.Card (Card)
import Web.ShoppingCart.Domain.Item (Money)
import Web.ShoppingCart.Domain.Order (OrderId, PaymentId)
import Web.ShoppingCart.Domain.Payment (Payment)
import Web.ShoppingCart.Domain.ShoppingCart (CartItem, CartTotal)
import Web.ShoppingCart.Domain.User (UserId)
import Web.ShoppingCart.Effects.Background (class Background, schedule)
import Web.ShoppingCart.Error (type (+), OrderCreateFailedError, PaymentFailedError, orderCreateFailedError, paymentFailedError)
import Web.ShoppingCart.Retry (retryPolicy, checks)
import Web.ShoppingCart.Services.Orders (Orders)
import Web.ShoppingCart.Services.Payments (Payments)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)

type CheckoutError r
  = Variant (OrderCreateFailedError + PaymentFailedError + r)

checkout ::
  forall r m.
  MonadAff m =>
  MonadError (CheckoutError r) m =>
  Background m =>
  Payments m ->
  ShoppingCart m ->
  Orders m ->
  UserId ->
  Card ->
  m OrderId
checkout paymentsClient shoppingCartClient ordersClient userId card = do
  cart <- shoppingCartClient.get userId
  paymentId <- processPayment paymentsClient (payment cart)
  orderId <- createOrder ordersClient userId paymentId (cart.items) (cart.total)
  void $ try $ shoppingCartClient.delete userId
  pure orderId
  where
  payment :: CartTotal -> Payment
  payment cart = { userId: userId, total: cart.total, card }

createOrder ::
  forall r m.
  MonadAff m =>
  MonadError (Variant (OrderCreateFailedError + r)) m =>
  Background m =>
  Orders m ->
  UserId ->
  PaymentId ->
  Array CartItem ->
  Money ->
  m OrderId
createOrder ordersClient userId paymentId cartItems cartTotal = backgroundAction $ recovering retryPolicy checks action
  where
  backgroundAction :: m OrderId -> m OrderId
  backgroundAction fa =
    fa
      `catchError`
        \err -> do
          liftEffect $ log "Creating order failed, rescheduling..."
          schedule (backgroundAction fa) (Milliseconds 10000.0)
          throwError orderCreateFailedError

  action :: RetryStatus -> m OrderId
  action _ = do
    liftEffect $ log "Creating order..."
    orderId <- liftEffect genUUID
    ordersClient.create (wrap $ toString orderId) userId paymentId cartItems cartTotal

processPayment ::
  forall r m.
  MonadAff m =>
  MonadError (Variant (PaymentFailedError + r)) m =>
  Payments m ->
  Payment ->
  m PaymentId
processPayment paymentsClient payment = recovering retryPolicy checks action
  where
  action :: RetryStatus -> m PaymentId
  action _ = do
    liftEffect $ log "Processing payment..."
    paymentsClient.process payment
      `catchError`
        \err -> do
          liftEffect $ log ("Failed to process payment..." <> show payment)
          throwError $ paymentFailedError (unwrap payment.userId) (unwrap payment.total)
