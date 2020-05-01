module Web.ShoppingCart.Services.Brands
  ( Brands(..)
  , mkBrands
  ) where

import Prelude
import Data.Show (show)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff)
import Effect.Class.Console (log, logShow)
import Selda.PG.Class (query)
import Selda.Query (selectFrom)
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database (generateSQLStringFromQuery, hoistSelda)
import Web.ShoppingCart.Database.Tables (brands)
import Web.ShoppingCart.Domain.Brand (Brand, BrandId(..), BrandName(..))

type Brands m
  = { findAll :: m (Array Brand)
    , create :: Brand -> m Unit
    }

type BrandDTO
  = { id :: String
    , name :: String
    }

mkBrands :: forall r. Brands (App r)
mkBrands =
  { findAll
  , create
  }

toBrand :: BrandDTO -> Brand
toBrand { id, name } = { brandId: BrandId id, brandName: BrandName name }

create :: forall r. Brand -> App r Unit
create brand =
  hoistSelda do
    log $ "Creating brand" <> show brand

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
    brandDTOs <- query sql
    pure $ map toBrand brandDTOs
