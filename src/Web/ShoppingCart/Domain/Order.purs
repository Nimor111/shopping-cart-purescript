module Web.ShoppingCart.Domain.Order
  ( OrderId(..)
  , PaymentId(..)
  , Order(..)
  , OrderItem(..)
  ) where

import Simple.JSON as JSON
import Data.Map (Map)
import Web.ShoppingCart.Domain.Item (ItemId(..), Money(..))
import Web.ShoppingCart.Domain.ShoppingCart (Quantity(..))
import Data.Newtype (class Newtype)

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
    , orderItems :: Array (OrderItem)
    , orderTotal :: Money
    }
