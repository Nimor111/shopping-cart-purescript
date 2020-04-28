module Web.ShoppingCart.Database
        ( hoistSelda
        , people
        , createPeople
        )
        where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT)
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (Variant, inj)
import Database.PostgreSQL (PGError)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff, error)
import Effect.Aff.Class (class MonadAff)
import Selda.Query.Class (hoistSeldaWith)
import Selda.Table (Table(..))
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Error (DatabaseError, databaseError, type (+))


hoistSelda
  :: ∀ e r1 r2 m
  .  MonadAsk { conn ∷ PostgreSQL.Connection | r1 } m
  => MonadThrow (Variant (DatabaseError + r2)) m
  => MonadAff m
  => ExceptT PGError (ReaderT PostgreSQL.Connection Aff) ~> m
hoistSelda = hoistSeldaWith databaseError (_.conn)

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
