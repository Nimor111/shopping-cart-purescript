module Web.ShoppingCart.Services.Auth
        ( Auth (..)
        ) where

import Prelude

import Data.Maybe (Maybe)

import Web.ShoppingCart.Domain.User (JwtToken, Password, User, UserName)


type Auth m =
  { findUser :: JwtToken -> m (Maybe User)
  , newUser :: UserName -> Password -> m JwtToken
  , login :: UserName -> Password -> m JwtToken
  {--, logout :: JwtToken -> UserName -> m Unit--}
  , logout :: JwtToken -> m Unit
  }
