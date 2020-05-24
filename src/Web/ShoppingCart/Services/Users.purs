module Web.ShoppingCart.Services.Users
  ( Users(..)
  ) where

import Prelude
import Control.Monad.Logger.Class (debug)
import Crypto.Bcrypt (Hash(..), compare, hash)
import Data.Array (head)
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Selda (restrict, selectFrom, (.==))
import Selda.Col (lit)
import Selda.PG.Class (insert1_, query)
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database (generateSQLStringFromQuery, hoistSelda)
import Web.ShoppingCart.Database.Tables (users)
import Web.ShoppingCart.Domain.User (Password(..), User, UserId(..), UserName(..))

type Users m
  = { find :: UserName -> Password -> m (Maybe User)
    , create :: UserId -> UserName -> Password -> m UserId
    }

type DBUser
  = { id :: String
    , userName :: String
    , password :: String
    }

mkUsers :: forall r. Users (App r)
mkUsers =
  { find
  , create
  }

toUser :: DBUser -> User
toUser { id, userName } = { id: UserId id, userName: UserName userName }

find :: forall r. UserName -> Password -> App r (Maybe User)
find (UserName name) (Password pass) = do
  let
    str = generateSQLStringFromQuery

    sql =
      selectFrom users \{ id, userName, password } -> do
        restrict $ userName .== (lit name)
        pure { id, userName, password }
  debug empty $ str sql
  hoistSelda do
    dbUser <- query sql
    passwordsEqual <- maybe (pure false) (\h -> liftAff $ comparePassword pass h.password) (head dbUser)
    if passwordsEqual then
      pure $ map toUser (head dbUser)
    else
      pure Nothing
  where
  comparePassword :: String -> String -> Aff Boolean
  comparePassword hash pass = compare (wrap hash) pass

create :: forall r. UserId -> UserName -> Password -> App r UserId
create userId userName password = do
  hashedPassword <- liftAff $ hashPassword password
  let
    userData = { id: unwrap userId, userName: unwrap userName, password: hashedPassword }
  debug empty $ "Creating user with id " <> unwrap userId <> " and name " <> unwrap userName
  hoistSelda do
    insert1_ users userData
    pure userId
  where
  hashPassword :: Password -> Aff String
  hashPassword pass = map unwrap $ hash 1 (unwrap pass)
