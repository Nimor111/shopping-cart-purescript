module Web.ShoppingCart.Domain.ShoppingCart
  ( Quantity(..)
  , Cart(..)
  , CartItem(..)
  , CartId(..)
  , CartTotal(..)
  , CartItemId(..)
  ) where

import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Item (Item, ItemId, Money(..))

newtype Quantity
  = Quantity Int

newtype Cart
  = Cart (Array CartItemId)

newtype CartId
  = CartId String

derive instance newtypeCart :: Newtype Cart _

derive instance newtypeQuantity :: Newtype Quantity _

derive instance newtypeCartId :: Newtype CartId _

derive newtype instance showCart :: Show Cart

derive newtype instance showQuantity :: Show Quantity

derive newtype instance showCartId :: Show CartId

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
