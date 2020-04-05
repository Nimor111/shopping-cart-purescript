module Web.ShoppingCart.Services.Users
        ( Users (..)
        ) where

import Data.Maybe (Maybe)

import Web.ShoppingCart.Domain.User (UserId, UserName, Password, User)


type Users m =
  { find :: UserName -> Password -> m (Maybe User)
  , create :: UserName -> Password -> m UserId
  }
