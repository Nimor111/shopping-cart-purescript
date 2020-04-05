module Web.ShoppingCart.Services.Payments
        ( class Payments
        , process
        ) where

import Web.ShoppingCart.Domain.Order (PaymentId)


class Payments m where
  process :: Payment -> m PaymentId
