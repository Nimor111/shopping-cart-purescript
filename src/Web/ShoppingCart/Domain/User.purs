module Web.ShoppingCart.Domain.User
  ( User(..)
  , UserId(..)
  , UserName(..)
  , LoginUser(..)
  , Password(..)
  , JwtToken(..)
  ) where

import Prelude
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (except)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=?), (~>), (:=), (~>?))
import Data.Newtype (class Newtype)
import Data.Show (class Show)

newtype UserId
  = UserId String

newtype UserName
  = UserName String

newtype Password
  = Password String

newtype JwtToken
  = JwtToken String

derive newtype instance showUserId :: Show UserId

derive instance newtypeUserId :: Newtype UserId _

derive instance newtypeUserName :: Newtype UserName _

derive instance newtypePassword :: Newtype Password _

derive instance newtypeJwtToken :: Newtype JwtToken _

derive newtype instance decodeJsonUserId :: DecodeJson UserId

derive newtype instance encodeJsonUserId :: EncodeJson UserId

derive newtype instance decodeJsonUserName :: DecodeJson UserName

derive newtype instance encodeJsonUserName :: EncodeJson UserName

derive newtype instance decodeJsonPassword :: DecodeJson Password

derive newtype instance encodeJsonPassword :: EncodeJson Password

derive newtype instance decodeJsonJwtToken :: DecodeJson JwtToken

derive newtype instance encodeJsonJwtToken :: EncodeJson JwtToken

type User
  = { userId :: UserId
    , userName :: UserName
    }

type LoginUser
  = { userName :: UserName
    , password :: Password
    }
