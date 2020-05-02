module Web.ShoppingCart.Services.Categories
  ( Categories(..)
  , mkCategories
  ) where

import Prelude
import Web.ShoppingCart.Domain.Category (Category)
import Data.Newtype (unwrap)
import Data.Show (show)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff)
import Effect.Class.Console (log, logShow)
import Selda.PG.Class (insert1_, query)
import Selda.Query (selectFrom)
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database (generateSQLStringFromQuery, hoistSelda)
import Web.ShoppingCart.Database.Tables (categories)
import Web.ShoppingCart.Domain.Category (Category, CategoryId(..), CategoryName(..))

type Categories m
  = { findAll :: m (Array Category)
    , create :: Category -> m Unit
    }

type DBCategory
  = { id :: String
    , name :: String
    }

mkCategories :: forall r. Categories (App r)
mkCategories =
  { findAll
  , create
  }

toCategory :: DBCategory -> Category
toCategory { id, name } = { id: CategoryId id, name: CategoryName name }

create :: forall r. Category -> App r Unit
create { id, name } =
  hoistSelda do
    let
      str = generateSQLStringFromQuery
    let
      categoryData = { id: unwrap id, name: unwrap name }
    insert1_ categories categoryData

findAll :: forall r. App r (Array Category)
findAll =
  hoistSelda do
    let
      str = generateSQLStringFromQuery
    let
      sql =
        selectFrom categories \{ id, name } -> do
          pure { id, name }
    log $ str sql
    dbCategories <- query sql
    pure $ map toCategory dbCategories
