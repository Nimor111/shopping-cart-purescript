module Web.ShoppingCart.Domain.ShoppingCart
  ( Quantity(..)
  , Cart(..)
  , CartItem(..)
  , CartId(..)
  , CartTotal(..)
  , CartItemId(..)
  ) where

import Data.Newtype (class Newtype)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Item (Item, ItemId, Money(..))

newtype Quantity
  = Quantity { unQuantity :: Int }

newtype Cart
  = Cart (Array CartItemId)

newtype CartId
  = CartId { unCartId :: String }

derive instance newtypeCart :: Newtype Cart _

derive newtype instance readForeignQuantity :: JSON.ReadForeign Quantity

derive newtype instance writeForeignQuantity :: JSON.WriteForeign Quantity

derive newtype instance readForeignCartId :: JSON.ReadForeign CartId

derive newtype instance writeForeignCartId :: JSON.WriteForeign CartId

derive newtype instance readForeignCart :: JSON.ReadForeign Cart

derive newtype instance writeForeignCart :: JSON.WriteForeign Cart

type CartItemId
  = { itemId :: ItemId
    , quantity :: Quantity
    }

type CartItem
  = { cartItem :: Item
    , cartItemQuantity :: Quantity
    }

type CartTotal
  = { cartTotalItems :: Array CartItem
    , cartTotal :: Money
    }
