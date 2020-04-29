module Web.ShoppingCart where

import Prelude
import Data.Either (Either(..))
import Database.PostgreSQL as PostgreSQL
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Web.ShoppingCart.Database (createPeople)
import Web.ShoppingCart.Server (server)

dbConfig :: PostgreSQL.PoolConfiguration
dbConfig =
  (PostgreSQL.defaultPoolConfiguration "shoppingcart")
    { user = Just "postgres"
    , password = Just ""
    }

runServer :: Effect Unit
runServer =
  launchAff_ do
    pool <- liftEffect $ PostgreSQL.newPool dbConfig
    PostgreSQL.withConnection pool case _ of
      Left pgError -> logShow ("PostgreSQL Connection Error: " <> show pgError)
      Right conn -> do
        log "Starting server..."
        createPeople conn
        log "Created people..."
        let
          config = { conn, other: "other", jwtSecret: "tOpSeCrEt" }
        void $ liftEffect $ server config
