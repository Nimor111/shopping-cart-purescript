module Web.ShoppingCart.Domain.Order
  ( OrderId(..)
  , PaymentId(..)
  , Order(..)
  , OrderItem(..)
  ) where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Web.ShoppingCart.Domain.Item (ItemId(..), Money(..))
import Web.ShoppingCart.Domain.ShoppingCart (Quantity(..))
import Web.ShoppingCart.Domain.User (User, UserId(..))

newtype OrderId
  = OrderId String

derive newtype instance showOrderId :: Show OrderId

newtype PaymentId
  = PaymentId String

derive newtype instance showPaymentId :: Show PaymentId

derive instance newtypeOrderId :: Newtype OrderId _

derive instance newtypePaymentId :: Newtype PaymentId _

derive newtype instance decodeJsonOrderId :: DecodeJson OrderId

derive newtype instance encodeJsonOrderId :: EncodeJson OrderId

derive newtype instance decodeJsonPaymentId :: DecodeJson PaymentId

derive newtype instance encodeJsonPaymentId :: EncodeJson PaymentId

type OrderItem
  = { itemId :: ItemId
    , quantity :: Quantity
    }

type Order
  = { id :: OrderId
    , paymentId :: PaymentId
    , userId :: UserId
    , items :: Array (OrderItem)
    , total :: Money
    }
