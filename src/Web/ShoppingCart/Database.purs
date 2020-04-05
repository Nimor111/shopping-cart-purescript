module Web.ShoppingCart.Database
        ( hoistSelda
        , people
        , createPeople
        , dbConfig
        )
        where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT)
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff, error)
import Effect.Aff.Class (class MonadAff)
import Selda.Query.Class (hoistSeldaWith)
import Selda.Table (Table(..))
import Database.PostgreSQL (PGError)
import Database.PostgreSQL as PostgreSQL

import Web.ShoppingCart.App (_pgError)


hoistSelda
  :: ∀ e r m
  .  MonadAsk { conn ∷ PostgreSQL.Connection | r } m
  => MonadThrow (Variant ( pgError ∷ PGError | e )) m
  => MonadAff m
  => ExceptT PGError (ReaderT PostgreSQL.Connection Aff) ~> m
hoistSelda = hoistSeldaWith (inj _pgError) (_.conn)

dbConfig :: PostgreSQL.PoolConfiguration
dbConfig = (PostgreSQL.defaultPoolConfiguration "shoppingcart")
  { user = Just "postgres"
  , password = Just ""
  }

execute :: String -> PostgreSQL.Connection -> Aff Unit
execute sql conn = do
    PostgreSQL.execute conn (PostgreSQL.Query sql) PostgreSQL.Row0
        >>= maybe (pure unit) (throwError <<< error <<< show)

createPeople ∷ PostgreSQL.Connection → Aff Unit
createPeople = execute """
  CREATE TABLE IF NOT EXISTS people (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER
  );"""

people ∷ Table
  ( id ∷ Int
  , name ∷ String
  , age ∷ Maybe Int
  )
people = Table { name: "people" }
