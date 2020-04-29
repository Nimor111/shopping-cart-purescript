module Web.ShoppingCart.Context
  ( Context
  ) where

import Database.PostgreSQL as PostgreSQL

type Context
  = { conn :: PostgreSQL.Connection
    , other :: String
    , jwtSecret :: String
    }
