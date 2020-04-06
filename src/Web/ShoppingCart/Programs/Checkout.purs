module Web.ShoppingCart.Programs.Checkout
        ( checkout
        ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, try)
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Retry (RetryStatus(..), recovering)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Exception (Error)
import Test.Unit.Console (log)
import Web.ShoppingCart.Domain.Card (Card)
import Web.ShoppingCart.Domain.Order (OrderId, PaymentId(..))
import Web.ShoppingCart.Domain.Payment (Payment)
import Web.ShoppingCart.Domain.ShoppingCart (CartTotal)
import Web.ShoppingCart.Domain.User (UserId)
import Web.ShoppingCart.Retry (retryPolicy, checks)
import Web.ShoppingCart.Services.Orders (Orders)
import Web.ShoppingCart.Services.Payments (Payments)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)


checkout
    :: forall m
    .  MonadAff m
    => MonadError Error m
    => Payments m
    -> ShoppingCart m
    -> Orders m
    -> UserId
    -> Card
    -> m OrderId
checkout p sc o userId card = do
    cart <- sc.get userId
    paymentId <- recovering retryPolicy checks (\_ -> p.process (payment cart))
    orderId <- o.create userId paymentId (cart.cartTotalItems) (cart.cartTotal)
    void $ try $ sc.delete userId

    pure orderId
        where
           payment :: CartTotal -> Payment
           payment cart = {paymentUserId: userId, paymentTotal: (cart.cartTotal), paymentCard: card}

processPayment
    :: forall m
    .  MonadAff m
    => MonadError Error m
    => Payments m
    -> Payment
    -> m PaymentId
processPayment pc payment = recovering retryPolicy checks action
    where
        action :: RetryStatus -> m PaymentId
        action _ = do
            liftEffect $ log "Processing payment..."
            pc.process payment
