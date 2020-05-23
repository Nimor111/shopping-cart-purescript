module Web.ShoppingCart.Services.Brands
  ( Brands(..)
  , mkBrands
  ) where

import Prelude

import Control.Monad.Logger.Class (debug)
import Data.Map.Internal (empty)
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
create { id, name } = do
  let
    str = generateSQLStringFromQuery
    brandData = { id: unwrap id, name: unwrap name }

  debug empty $ "Creating new brand with id " <> unwrap id <> " and name " <> unwrap name
  hoistSelda do
    insert1_ brands brandData

findAll :: forall r. App r (Array Brand)
findAll = do
  let
    str = generateSQLStringFromQuery
    sql =
      selectFrom brands \{ id, name } -> do
        pure { id, name }

  debug empty $ str sql
  hoistSelda do
    dbBrands <- query sql
    pure $ map toBrand dbBrands
