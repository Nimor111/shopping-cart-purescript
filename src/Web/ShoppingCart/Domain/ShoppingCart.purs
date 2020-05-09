module Web.ShoppingCart.Domain.ShoppingCart
  ( Quantity(..)
  , Cart(..)
  , CartItem(..)
  , CartId(..)
  , CartTotal(..)
  , CartItemId(..)
  ) where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
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

derive newtype instance decodeJsonQuantity :: DecodeJson Quantity

derive newtype instance encodeJsonQuantity :: EncodeJson Quantity

derive newtype instance decodeJsonCartId :: DecodeJson CartId

derive newtype instance encodeJsonCartId :: EncodeJson CartId

derive newtype instance decodeJsonCart :: DecodeJson Cart

derive newtype instance encodeJsonCart :: EncodeJson Cart

type CartItemId
  = { itemId :: ItemId
    , quantity :: Quantity
    }

type CartItem
  = { item :: Item
    , quantity :: Quantity
    }

type CartTotal
  = { items :: Array CartItem
    , total :: Money
    }
