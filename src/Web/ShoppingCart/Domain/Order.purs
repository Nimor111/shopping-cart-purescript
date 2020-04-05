module Web.ShoppingCart.Domain.Order
        ( OrderId (..)
        , PaymentId (..)
        , Order (..)
        ) where

import Data.Map (Map)
import Data.UUID (UUID)
import Web.ShoppingCart.Domain.Item (ItemId(..), Money(..))
import Web.ShoppingCart.Domain.ShoppingCart (Quantity(..))


newtype OrderId = OrderId { unOrderId :: UUID }
newtype PaymentId = PaymentId { unPaymentId :: UUID }

data Order = Order
    { orderId :: OrderId
    , orderPaymentId :: PaymentId
    , orderItems :: Map ItemId Quantity
    , orderTotal :: Money
    }
