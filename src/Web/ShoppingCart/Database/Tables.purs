module Web.ShoppingCart.Database.Tables
  ( createTables
  , dropTables
  , brands
  , categories
  , items
  , orders
  , users
  ) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger, info)
import Data.Argonaut.Core (Json)
import Data.Map.Internal (empty)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Selda.Table (Table(..))
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database (execute)

createTables :: forall r. PostgreSQL.Connection -> App r Unit
createTables conn = do
  info empty $ "Creating tables..."
  createBrands conn
  createCategories conn
  createItems conn
  createUsers conn
  createOrders conn
  info empty $ "Tables created."

dropTables :: forall r. PostgreSQL.Connection -> App r Unit
dropTables conn = do
  info empty $ "Dropping tables..."
  dropItems conn
  dropOrders conn
  dropUsers conn
  dropBrands conn
  dropCategories conn
  info empty $ "Tables dropped."

brands :: Table ( id :: String, name :: String )
brands = Table { name: "brands" }

createBrands :: forall r. PostgreSQL.Connection -> App r Unit
createBrands =
  liftAff <<< execute
    """
  CREATE TABLE IF NOT EXISTS brands (
    id UUID PRIMARY KEY,
    name TEXT NOT NULL
  );
"""

dropBrands :: forall r. PostgreSQL.Connection -> App r Unit
dropBrands =
  liftAff <<< execute
    """
    DROP TABLE brands;
"""

categories :: Table ( id :: String, name :: String )
categories = Table { name: "categories" }

createCategories :: forall r. PostgreSQL.Connection -> App r Unit
createCategories =
  liftAff <<< execute
    """
  CREATE TABLE IF NOT EXISTS categories (
    id UUID PRIMARY KEY,
    name TEXT NOT NULL
  );
"""

dropCategories :: forall r. PostgreSQL.Connection -> App r Unit
dropCategories =
  liftAff <<< execute
    """
    DROP TABLE categories;
"""

items :: Table ( id :: String, name :: String, description :: String, price :: Int, brandId :: String, categoryId :: String )
items = Table { name: "items" }

-- TODO: make price a REAL
createItems :: forall r. PostgreSQL.Connection -> App r Unit
createItems =
  liftAff <<< execute
    """
  CREATE TABLE IF NOT EXISTS items (
    id UUID PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    price INTEGER NOT NULL,
    brandId UUID NOT NULL,
    categoryId UUID NOT NULL,
    CONSTRAINT brand_id_fkey FOREIGN KEY (brandId)
      REFERENCES brands (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
    CONSTRAINT category_id_fkey FOREIGN KEY (categoryId)
      REFERENCES categories (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
  );
"""

dropItems :: forall r. PostgreSQL.Connection -> App r Unit
dropItems =
  liftAff <<< execute
    """
    DROP TABLE items;
"""

orders :: Table ( id :: String, total :: Int, userId :: String, paymentId :: String, items :: Json )
orders = Table { name: "orders" }

createOrders :: forall r. PostgreSQL.Connection -> App r Unit
createOrders =
  liftAff <<< execute
    """
  CREATE TABLE IF NOT EXISTS orders (
    id UUID PRIMARY KEY,
    paymentId UUID UNIQUE NOT NULL,
    quantity INTEGER NOT NULL,
    userId UUID NOT NULL,
    items JSONB NOT NULL,
    CONSTRAINT user_id_fkey FOREIGN KEY (userId)
      REFERENCES users (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
  );
"""

dropOrders :: forall r. PostgreSQL.Connection -> App r Unit
dropOrders =
  liftAff <<< execute
    """
    DROP TABLE orders;
"""

users :: Table ( id :: String, userName :: String, password :: String )
users = Table { name: "users" }

createUsers :: forall r. PostgreSQL.Connection -> App r Unit
createUsers =
  liftAff <<< execute
    """
  CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY,
    username TEXT NOT NULL,
    password TEXT NOT NULL
  );
"""

dropUsers :: forall r. PostgreSQL.Connection -> App r Unit
dropUsers =
  liftAff <<< execute
    """
    DROP TABLE users;
"""
