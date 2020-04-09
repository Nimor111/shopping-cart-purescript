module Web.ShoppingCart.Domain.User
      ( User (..)
      , UserId (..)
      , UserName (..)
      , Password (..)
      , JwtToken (..)
      ) where

import Simple.JSON as JSON


newtype UserId = UserId { unUserId :: String }
newtype UserName = UserName { unUserName :: String }
newtype Password = Password { unPassword :: String }
newtype JwtToken = JwtToken { unJwtToken :: String }

derive newtype instance readForeignUserId :: JSON.ReadForeign UserId
derive newtype instance writeForeignUserId :: JSON.WriteForeign UserId

derive newtype instance readForeignUserName :: JSON.ReadForeign UserName
derive newtype instance writeForeignUserName :: JSON.WriteForeign UserName

derive newtype instance readForeignPassword :: JSON.ReadForeign Password
derive newtype instance writeForeignPassword :: JSON.WriteForeign Password

derive newtype instance readForeignJwtToken :: JSON.ReadForeign JwtToken
derive newtype instance writeForeignJwtToken :: JSON.WriteForeign JwtToken

type User =
  { userId :: UserId
  , userName :: UserName
  }
