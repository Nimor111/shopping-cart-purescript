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

type CartItem =
    { cartItem :: Item
    , cartItemQuantity :: Quantity
    }

type CartTotal =
    { cartTotalItems :: List CartItem
    , cartTotal :: Money
    }
