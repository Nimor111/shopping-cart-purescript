module Web.ShoppingCart.Domain.User
      ( User (..)
      , UserId (..)
      , UserName (..)
      , Password (..)
      , JwtToken (..)
      ) where

import Data.UUID (UUID)


newtype UserId = UserId { unUserId :: UUID }
newtype UserName = UserName { unUserName :: String }
newtype Password = Password { unPassword :: String }
newtype JwtToken = JwtToken { unJwtToken :: String }

type User =
  { userId :: UserId
  , userName :: UserName
  }
