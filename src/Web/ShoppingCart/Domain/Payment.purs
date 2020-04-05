module Web.ShoppingCart.Domain.Payment
        ( Payment (..)
        ) where

import Web.ShoppingCart.Domain.Item (Money)
import Web.ShoppingCart.Domain.User (UserId)
import Web.ShoppingCart.Domain.Card (Card)


type Payment =
  { paymentUserId :: UserId
  , paymentTotal :: Money
  , paymentCard :: Card
  }
