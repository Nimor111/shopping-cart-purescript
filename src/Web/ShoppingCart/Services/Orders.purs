module Web.ShoppingCart.Services.Orders
        ( Orders (..)
        ) where

import Data.List.Types (List)
import Data.Maybe (Maybe)
import Web.ShoppingCart.Domain.Item (Money)
import Web.ShoppingCart.Domain.Order (Order, OrderId, PaymentId)
import Web.ShoppingCart.Domain.ShoppingCart (CartItem)
import Web.ShoppingCart.Domain.User (UserId)


type Orders m =
  { get :: UserId -> OrderId -> m (Maybe Order)
  , findBy :: UserId -> m (List Order)
  , create :: UserId -> PaymentId -> List CartItem -> Money -> m OrderId
  }
