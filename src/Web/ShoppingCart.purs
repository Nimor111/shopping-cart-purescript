module Web.ShoppingCart where

import Prelude

import Control.Monad.Logger.Class (error, info)
import Data.Either (Either(..))
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL as PostgreSQL
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Web.ShoppingCart.App (App, runApp)
import Web.ShoppingCart.Database.Tables (createTables, dropTables)
import Web.ShoppingCart.Server (Services, server)

dbConfig :: PostgreSQL.PoolConfiguration
dbConfig =
  (PostgreSQL.defaultPoolConfiguration "shoppingcart")
    { user = Just "postgres"
    , password = Just ""
    }

runServer :: forall r. Services (App r) -> Effect Unit
runServer services = launchAff_ do
  pool <- liftEffect $ PostgreSQL.newPool dbConfig
  PostgreSQL.withConnection pool case _ of
    {- TODO: For this to work we need an empty context of sorts
    Which would mean the connection in the config to be a Maybe
    Left pgError -> runApp emptyContext $ error empty $ ("PostgreSQL Connection Error: " <> show pgError)
    -}
    Left pgError -> log $ ("PostgreSQL Connection Error: " <> show pgError)
    Right conn -> do
      let
        config = { conn, other: "other", jwtSecret: "tOpSeCrEt" }
      void $ runApp config $ info empty $ "Starting server..."
      -- TODO: don't discard errors here
      void $ runApp config $ createTables conn
      void $ liftEffect $ server config services
