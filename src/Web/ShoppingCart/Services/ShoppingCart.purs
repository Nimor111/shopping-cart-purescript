module Web.ShoppingCart.Services.ShoppingCart
  ( ShoppingCart(..)
  ) where

import Prelude
import Web.ShoppingCart.Domain.Item (ItemId)
import Web.ShoppingCart.Domain.ShoppingCart (CartTotal, Quantity, Cart)
import Web.ShoppingCart.Domain.User (UserId)

type ShoppingCart m
  = { get :: UserId -> m CartTotal
    , add :: UserId -> ItemId -> Quantity -> m Unit
    , delete :: UserId -> m Unit
    , removeItem :: UserId -> ItemId -> m Unit
    , update :: UserId -> Cart -> m Unit
    }
