module Web.ShoppingCart.Services.Payments
        ( Payments (..)
        ) where

import Web.ShoppingCart.Domain.Order (PaymentId)
import Web.ShoppingCart.Domain.Payment (Payment)


type Payments m =
  { process :: Payment -> m PaymentId
  }
