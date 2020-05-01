module Web.ShoppingCart.Database
  ( hoistSelda
  , execute
  , generateSQLStringFromQuery
  ) where

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
import Selda.Col (class GetCols)
import Selda.PG (showPG)
import Selda.Query.Class (hoistSeldaWith)
import Selda.Query.ShowStatement (showQuery)
import Selda.Query.Type (FullQuery(..))
import Selda.Table (Table(..))
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Error (DatabaseError, databaseError, type (+))

hoistSelda ::
  ∀ e r1 r2 m.
  MonadAsk { conn ∷ PostgreSQL.Connection | r1 } m =>
  MonadThrow (Variant (DatabaseError + r2)) m =>
  MonadAff m =>
  ExceptT PGError (ReaderT PostgreSQL.Connection Aff) ~> m
hoistSelda = hoistSeldaWith databaseError (_.conn)

execute :: String -> PostgreSQL.Connection -> Aff Unit
execute sql conn = do
  PostgreSQL.execute conn (PostgreSQL.Query sql) PostgreSQL.Row0
    >>= maybe (pure unit) (throwError <<< error <<< show)

generateSQLStringFromQuery ∷
  ∀ s r.
  GetCols r ⇒
  FullQuery s { | r } →
  String
generateSQLStringFromQuery = showQuery >>> showPG >>> _.strQuery
