module Web.ShoppingCart.Domain.Payment
  ( Payment(..)
  ) where

import Web.ShoppingCart.Domain.Card (Card)
import Web.ShoppingCart.Domain.Item (Money)
import Web.ShoppingCart.Domain.User (UserId)

type Payment
  = { userId :: UserId
    , total :: Money
    , card :: Card
    }
