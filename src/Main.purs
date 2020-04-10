module Main where

import Prelude

import Control.Bind (join)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Database.PostgreSQL as PostgreSQL
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Web.ShoppingCart.Database (dbConfig, createPeople)
import Web.ShoppingCart.Server (server)


main :: Effect Unit
main = launchAff_ do
    pool <- liftEffect $ PostgreSQL.newPool dbConfig
    PostgreSQL.withConnection pool case _ of
      Left pgError -> logShow ("PostgreSQL Connection Error: " <> show pgError)
      Right conn -> do
         log "Starting server..."
         createPeople conn
         log "Created people..."

         void $ liftEffect $ server { conn, other: "other" }
