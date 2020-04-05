module Web.ShoppingCart.Services.ShoppingCart
        ( class ShoppingCart
        , get
        , add
        , delete
        , removeItem
        , update
        ) where

import Prelude

import Web.ShoppingCart.Domain.User (UserId)
import Web.ShoppingCart.Domain.ShoppingCart (CartTotal, Quantity, Cart)
import Web.ShoppingCart.Domain.Item (ItemId)


class ShoppingCart m where
    get :: UserId -> m CartTotal
    add :: UserId -> ItemId -> Quantity -> m Unit
    delete :: UserId -> m Unit
    removeItem :: UserId -> ItemId -> m Unit
    update :: UserId -> Cart -> m Unit
