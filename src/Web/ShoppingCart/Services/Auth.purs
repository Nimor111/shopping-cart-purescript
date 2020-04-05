module Web.ShoppingCart.Services.Auth
        ( class Auth
        , findUser
        , newUser
        , login
        , logout
        ) where

import Prelude

import Data.Maybe (Maybe)

import Web.ShoppingCart.Domain.User (JwtToken, Password, User, UserName)


class Auth m where
  findUser :: JwtToken -> m (Maybe User)
  newUser :: UserName -> Password -> m JwtToken
  login :: UserName -> Password -> m JwtToken
  logout :: JwtToken -> UserName -> m Unit
