module Web.ShoppingCart.Domain.Order
        ( OrderId (..)
        , PaymentId (..)
        , Order (..)
        ) where

import Simple.JSON as JSON

import Data.Map (Map)
import Web.ShoppingCart.Domain.Item (ItemId(..), Money(..))
import Web.ShoppingCart.Domain.ShoppingCart (Quantity(..))


newtype OrderId = OrderId { unOrderId :: String }
newtype PaymentId = PaymentId { unPaymentId :: String }

derive newtype instance readForeignOrderId :: JSON.ReadForeign OrderId
derive newtype instance writeForeignOrderId :: JSON.WriteForeign OrderId

derive newtype instance readForeignPaymentId :: JSON.ReadForeign PaymentId
derive newtype instance writeForeignPaymentId :: JSON.WriteForeign PaymentId

type Order =
    { orderId :: OrderId
    , orderPaymentId :: PaymentId
    , orderItems :: Map ItemId Quantity
    , orderTotal :: Money
    }
