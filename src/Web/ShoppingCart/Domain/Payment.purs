module Web.ShoppingCart.Domain.Payment
        ( Payment (..)
        ) where

import Web.ShoppingCart.Domain.Item (Money)
import Web.ShoppingCart.Domain.User (UserId)


data Payment = Payment
  { paymentUserId :: UserId
  , paymentTotal :: Money
  , card :: Card
  }
