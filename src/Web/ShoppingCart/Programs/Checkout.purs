module Web.ShoppingCart.Programs.Checkout
        ( checkout
        ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Exception (error)

import Control.Monad.Error.Class (class MonadError, try, catchError, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Retry (RetryStatus, recovering)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Test.Unit.Console (log)
import Web.ShoppingCart.Domain.Card (Card)
import Web.ShoppingCart.Domain.Item (Money)
import Web.ShoppingCart.Domain.Order (OrderId, PaymentId)
import Web.ShoppingCart.Domain.Payment (Payment)
import Web.ShoppingCart.Domain.ShoppingCart (CartItem, CartTotal)
import Web.ShoppingCart.Domain.User (UserId)
import Web.ShoppingCart.Retry (retryPolicy, checks)
import Web.ShoppingCart.Services.Orders (Orders)
import Web.ShoppingCart.Services.Payments (Payments)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)
import Web.ShoppingCart.Effects.Background (class Background, schedule)

data OrderError = OrderError

checkout
    :: forall e m
    .  MonadAff m
    => MonadError e m
    => Background m
    => Payments m
    -> ShoppingCart m
    -> Orders m
    -> UserId
    -> Card
    -> m OrderId
checkout p sc o userId card = do
    cart <- sc.get userId
    paymentId <- processPayment p (payment cart)
    orderId <- createOrder o userId paymentId (cart.cartTotalItems) (cart.cartTotal)
    void $ try $ sc.delete userId

    pure orderId
    where
       payment :: CartTotal -> Payment
       payment cart = {paymentUserId: userId, paymentTotal: (cart.cartTotal), paymentCard: card}

createOrder
    :: forall e m
    .  MonadAff m
    => MonadError e m
    => Background m
    => Orders m
    -> UserId
    -> PaymentId
    -> Array CartItem
    -> Money
    -> m OrderId
createOrder o userId paymentId cartItems cartTotal = backgroundAction $ recovering retryPolicy checks action
    where
        backgroundAction :: m OrderId -> m OrderId
        backgroundAction fa = fa `catchError` \err -> do
            liftEffect $ log "Creating order failed, rescheduling..."
            schedule (backgroundAction fa) (Milliseconds 10000.0)
            throwError err

        action :: RetryStatus -> m OrderId
        action _ = do
            liftEffect $ log "Creating order..."
            o.create userId paymentId cartItems cartTotal

processPayment
    :: forall e m
    .  MonadAff m
    => MonadError e m
    => Payments m
    -> Payment
    -> m PaymentId
processPayment pc payment = recovering retryPolicy checks action
    where
        action :: RetryStatus -> m PaymentId
        action _ = do
            liftEffect $ log "Processing payment..."
            pc.process payment
