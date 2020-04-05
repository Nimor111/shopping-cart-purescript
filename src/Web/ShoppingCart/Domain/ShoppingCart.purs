module Web.ShoppingCart.Domain.ShoppingCart
        ( Quantity (..)
        , Cart (..)
        , CartItem (..)
        , CartId (..)
        , CartTotal (..)
        ) where

import Data.List.Types (List)
import Data.Map (Map)
import Data.UUID (UUID)
import Web.ShoppingCart.Domain.Item (Item, ItemId, Money(..))


newtype Quantity = Quantity { unQuantity :: Int }
newtype Cart = Cart { unCart :: Map ItemId Quantity }
newtype CartId = CartId { unCartId :: UUID }

data CartItem = CartItem
    { cartItem :: Item
    , cartItemQuantity :: Quantity
    }

data CartTotal = CartTotal
    { cartTotalItems :: List CartItem
    , cartTotal :: Money
    }
