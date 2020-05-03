module Web.ShoppingCart.Services.Items
  ( Items(..)
  , mkItems
  ) where

import Prelude
import Data.Array (head)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Show (show)
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff)
import Effect.Class.Console (log, logShow)
import Selda (leftJoin, restrict, selectFrom, (.==))
import Selda.Col (lit)
import Selda.PG.Class (insert1_, query)
import Selda.PG.Class (update) as SeldaPG
import Selda.Query (innerJoin)
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database (generateSQLStringFromQuery, hoistSelda)
import Web.ShoppingCart.Database.Tables (brands, items)
import Web.ShoppingCart.Domain.Brand (BrandId(..), BrandName(..))
import Web.ShoppingCart.Domain.Category (CategoryId(..))
import Web.ShoppingCart.Domain.Item (CreateItem, Item, ItemDescription(..), ItemId(..), ItemName(..), Money(..), UpdateItem)

type Items m
  = { findAll :: m (Array Item)
    , findBy :: BrandName -> m (Array Item)
    , findById :: ItemId -> m (Maybe Item)
    , create :: CreateItem -> m Unit
    , update :: UpdateItem -> m Unit
    }

type DBItem
  = { id :: String
    , name :: String
    , description :: String
    , price :: Int
    , brandId :: String
    , categoryId :: String
    }

mkItems :: forall r. Items (App r)
mkItems =
  { findAll
  , findBy
  , findById
  , create
  , update
  }

toItem :: DBItem -> Item
toItem { id, name, description, price, brandId, categoryId } = { id: ItemId id, name: ItemName name, description: ItemDescription description, price: Money price, brandId: BrandId brandId, categoryId: CategoryId categoryId }

findAll :: forall r. App r (Array Item)
findAll =
  hoistSelda do
    let
      str = generateSQLStringFromQuery
    let
      sql =
        selectFrom items \{ id, name, description, price, brandId, categoryId } -> do
          pure { id, name, description, price, brandId, categoryId }
    log $ str sql
    dbItems <- query sql
    pure $ map toItem dbItems

findBy :: forall r. BrandName -> App r (Array Item)
findBy (BrandName brandName) =
  hoistSelda do
    let
      str = generateSQLStringFromQuery
    let
      sql =
        selectFrom items \{ id, name, description, price, brandId, categoryId } -> do
          b <-
            innerJoin brands \brand -> brandId .== brand.id
          restrict $ b.name .== (lit brandName)
          pure { id, name, description, price, brandId, categoryId }
    log $ str sql
    dbItems <- query sql
    pure $ map toItem dbItems

findById :: forall r. ItemId -> App r (Maybe Item)
findById (ItemId itemId) =
  hoistSelda do
    let
      str = generateSQLStringFromQuery
    let
      sql =
        selectFrom items \{ id, name, description, price, brandId, categoryId } -> do
          restrict $ (lit itemId) .== id
          pure $ { id, name, description, price, brandId, categoryId }
    log $ str sql
    dbItems <- query sql
    pure $ map toItem $ head dbItems

create :: forall r. CreateItem -> App r Unit
create { id, name, description, price, brandId, categoryId } =
  hoistSelda do
    let
      str = generateSQLStringFromQuery
    let
      itemData = { id: unwrap id, name: unwrap name, description: unwrap description, price: unwrap price, brandId: unwrap brandId, categoryId: unwrap categoryId }
    insert1_ items itemData

update :: forall r. UpdateItem -> App r Unit
update { id, price } =
  hoistSelda do
    SeldaPG.update items
      (\r -> r.id .== (lit $ unwrap id))
      (\r -> r { price = lit $ unwrap price })
