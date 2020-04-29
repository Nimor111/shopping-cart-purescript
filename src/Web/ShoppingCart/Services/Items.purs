module Web.ShoppingCart.Services.Items
  ( Items(..)
  ) where

import Prelude
import Data.Maybe (Maybe)
import Web.ShoppingCart.Domain.Brand (BrandName)
import Web.ShoppingCart.Domain.Item (Item, ItemId, CreateItem, UpdateItem)

type Items m
  = { findAll :: m (Array Item)
    , findBy :: BrandName -> m (Array Item)
    , findById :: ItemId -> m (Maybe Item)
    , create :: CreateItem -> m Unit
    , update :: UpdateItem -> m Unit
    }
