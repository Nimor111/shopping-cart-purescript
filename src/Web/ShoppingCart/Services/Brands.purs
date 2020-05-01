module Web.ShoppingCart.Services.Brands
  ( Brands(..)
  , mkBrands
  ) where

import Prelude
import Data.Newtype (unwrap)
import Data.Show (show)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff)
import Effect.Class.Console (log, logShow)
import Selda.PG.Class (insert1_, query)
import Selda.Query (selectFrom)
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database (generateSQLStringFromQuery, hoistSelda)
import Web.ShoppingCart.Database.Tables (brands)
import Web.ShoppingCart.Domain.Brand (Brand, BrandId(..), BrandName(..))

type Brands m
  = { findAll :: m (Array Brand)
    , create :: Brand -> m Unit
    }

type DBBrand
  = { id :: String
    , name :: String
    }

mkBrands :: forall r. Brands (App r)
mkBrands =
  { findAll
  , create
  }

toBrand :: DBBrand -> Brand
toBrand { id, name } = { id: BrandId id, name: BrandName name }

create :: forall r. Brand -> App r Unit
create { id, name } =
  hoistSelda do
    let
      str = generateSQLStringFromQuery
    let
      brandData = { id: unwrap id, name: unwrap name }
    insert1_ brands brandData

findAll :: forall r. App r (Array Brand)
findAll =
  hoistSelda do
    let
      str = generateSQLStringFromQuery
    let
      sql =
        selectFrom brands \{ id, name } -> do
          pure { id, name }
    log $ str sql
    dbBrands <- query sql
    pure $ map toBrand dbBrands
