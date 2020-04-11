module Web.ShoppingCart.Domain.User
      ( User (..)
      , UserId (..)
      , UserName (..)
      , Password (..)
      , JwtToken (..)
      ) where

import Data.Newtype (class Newtype)
import Simple.JSON as JSON


newtype UserId = UserId String
newtype UserName = UserName String
newtype Password = Password String
newtype JwtToken = JwtToken String

derive instance newtypeUserId   :: Newtype UserId   _
derive instance newtypeUserName :: Newtype UserName _
derive instance newtypePassword :: Newtype Password _
derive instance newtypeJwtToken :: Newtype JwtToken _

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
