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
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT)
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (Variant, inj)
import Database.PostgreSQL (PGError)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff, error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Selda.Query.Class (hoistSeldaWith)
import Selda.Table (Table(..))
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Database (execute)
import Web.ShoppingCart.Domain.Brand (Brand, BrandId(..), BrandName(..))
import Web.ShoppingCart.Domain.Category (Category, CategoryId(..), CategoryName(..))
import Web.ShoppingCart.Domain.Order (OrderItem)
import Web.ShoppingCart.Error (DatabaseError, databaseError, type (+))

createTables :: PostgreSQL.Connection -> Aff Unit
createTables conn = do
  log "Creating tables..."
  createBrands conn
  createCategories conn
  createItems conn
  createUsers conn
  createOrders conn
  log "Tables created."

dropTables :: PostgreSQL.Connection -> Aff Unit
dropTables conn = do
  log "Dropping tables..."
  dropItems conn
  dropOrders conn
  dropUsers conn
  dropBrands conn
  dropCategories conn
  log "Tables dropped."

brands :: Table ( id :: String, name :: String )
brands = Table { name: "brands" }

createBrands :: PostgreSQL.Connection -> Aff Unit
createBrands =
  execute
    """
  CREATE TABLE IF NOT EXISTS brands (
    id UUID PRIMARY KEY,
    name TEXT NOT NULL
  );
"""

dropBrands :: PostgreSQL.Connection -> Aff Unit
dropBrands =
  execute
    """
    DROP TABLE brands;
"""

categories :: Table ( id :: String, name :: String )
categories = Table { name: "categories" }

createCategories :: PostgreSQL.Connection -> Aff Unit
createCategories =
  execute
    """
  CREATE TABLE IF NOT EXISTS categories (
    id UUID PRIMARY KEY,
    name TEXT NOT NULL
  );
"""

dropCategories :: PostgreSQL.Connection -> Aff Unit
dropCategories =
  execute
    """
    DROP TABLE categories;
"""

items :: Table ( id :: String, name :: String, description :: String, price :: Int, brand :: String, category :: String )
items = Table { name: "items" }

createItems :: PostgreSQL.Connection -> Aff Unit
createItems =
  execute
    """
  CREATE TABLE IF NOT EXISTS items (
    id UUID PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    price NUMERIC NOT NULL,
    brand_id UUID NOT NULL,
    category_id UUID NOT NULL,
    CONSTRAINT brand_id_fkey FOREIGN KEY (brand_id)
      REFERENCES brands (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
    CONSTRAINT category_id_fkey FOREIGN KEY (category_id)
      REFERENCES categories (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
  );
"""

dropItems :: PostgreSQL.Connection -> Aff Unit
dropItems =
  execute
    """
    DROP TABLE items;
"""

orders :: Table ( id :: String, total :: Int, userId :: String, paymentId :: String, items :: Array (OrderItem) )
orders = Table { name: "orders" }

createOrders :: PostgreSQL.Connection -> Aff Unit
createOrders =
  execute
    """
  CREATE TABLE IF NOT EXISTS orders (
    id UUID PRIMARY KEY,
    payment_id UUID UNIQUE NOT NULL,
    quantity NUMERIC NOT NULL,
    user_id UUID NOT NULL,
    items JSONB NOT NULL,
    CONSTRAINT user_id_fkey FOREIGN KEY (user_id)
      REFERENCES users (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
  );
"""

dropOrders :: PostgreSQL.Connection -> Aff Unit
dropOrders =
  execute
    """
    DROP TABLE orders;
"""

users :: Table ( id :: String, userName :: String, password :: String )
users = Table { name: "users" }

createUsers :: PostgreSQL.Connection -> Aff Unit
createUsers =
  execute
    """
  CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY,
    username TEXT NOT NULL,
    password TEXT NOT NULL
  );
"""

dropUsers :: PostgreSQL.Connection -> Aff Unit
dropUsers =
  execute
    """
    DROP TABLE users;
"""