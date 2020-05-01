module Web.ShoppingCart.Domain.Order
  ( OrderId(..)
  , PaymentId(..)
  , Order(..)
  , OrderItem(..)
  ) where

import Data.Map (Map)
import Data.Newtype (class Newtype)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Item (ItemId(..), Money(..))
import Web.ShoppingCart.Domain.ShoppingCart (Quantity(..))
import Web.ShoppingCart.Domain.User (User, UserId(..))

newtype OrderId
  = OrderId String

newtype PaymentId
  = PaymentId String

derive instance newtypeOrderId :: Newtype OrderId _

derive instance newtypePaymentId :: Newtype PaymentId _

derive newtype instance readForeignOrderId :: JSON.ReadForeign OrderId

derive newtype instance writeForeignOrderId :: JSON.WriteForeign OrderId

derive newtype instance readForeignPaymentId :: JSON.ReadForeign PaymentId

derive newtype instance writeForeignPaymentId :: JSON.WriteForeign PaymentId

type OrderItem
  = { itemId :: ItemId
    , quantity :: Quantity
    }

type Order
  = { orderId :: OrderId
    , orderPaymentId :: PaymentId
    , orderUserId :: UserId
    , orderItems :: Array (OrderItem)
    , orderTotal :: Money
    }
