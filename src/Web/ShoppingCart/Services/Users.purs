module Web.ShoppingCart.Services.Users
        ( class Users
        , find
        , create
        ) where

import Data.Maybe (Maybe)

import Web.ShoppingCart.Domain.User (UserId, UserName, Password, User)


class Users m where
  find :: UserName -> Password -> m (Maybe User)
  create :: UserName -> Password -> m UserId
