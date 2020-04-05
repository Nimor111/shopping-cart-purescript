module Web.ShoppingCart.Services.Users
        ( Users (..)
        ) where

import Data.Maybe (Maybe)

import Web.ShoppingCart.Domain.User (UserId, UserName, Password, User)


data Users m = Users
  { find :: UserName -> Password -> m (Maybe User)
  , create :: UserName -> Password -> m UserId
  }
