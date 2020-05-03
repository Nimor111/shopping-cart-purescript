module Web.ShoppingCart where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Database.PostgreSQL as PostgreSQL
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database.Tables (createTables, dropTables)
import Web.ShoppingCart.Server (Services, server)

dbConfig :: PostgreSQL.PoolConfiguration
dbConfig =
  (PostgreSQL.defaultPoolConfiguration "shoppingcart")
    { user = Just "postgres"
    , password = Just ""
    }

runServer :: forall r. Services (App r) -> Effect Unit
runServer services =
  launchAff_ do
    pool <- liftEffect $ PostgreSQL.newPool dbConfig
    PostgreSQL.withConnection pool case _ of
      Left pgError -> logShow ("PostgreSQL Connection Error: " <> show pgError)
      Right conn -> do
        log "Starting server..."
        createTables conn
        let
          config = { conn, other: "other", jwtSecret: "tOpSeCrEt" }
        void $ liftEffect $ server config services
