module Web.ShoppingCart.Programs.Checkout
        ( checkout
        ) where

import Prelude

import Web.ShoppingCart.Domain.Card (Card)
import Web.ShoppingCart.Domain.Order (OrderId)
import Web.ShoppingCart.Domain.Payment (Payment)
import Web.ShoppingCart.Domain.User (UserId)
import Web.ShoppingCart.Domain.ShoppingCart (CartTotal)
import Web.ShoppingCart.Services.Orders (Orders)
import Web.ShoppingCart.Services.Payments (Payments)

import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)


checkout
    :: forall m
    .  Monad m
    => Payments m
    -> ShoppingCart m
    -> Orders m
    -> UserId
    -> Card
    -> m OrderId
checkout p sc o userId card = do
    cart <- sc.get userId
    paymentId <- p.process (payment cart)
    orderId <- o.create userId paymentId (cart.cartTotalItems) (cart.cartTotal)
    sc.delete userId

    pure orderId
        where
           payment :: CartTotal -> Payment
           payment cart = {paymentUserId: userId, paymentTotal: (cart.cartTotal), paymentCard: card}
