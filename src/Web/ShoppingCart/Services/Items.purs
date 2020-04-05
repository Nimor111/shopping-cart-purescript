module Web.ShoppingCart.Services.Items
        ( Items (..)
        ) where

import Prelude

import Data.List.Types (List)
import Data.Maybe (Maybe)
import Web.ShoppingCart.Domain.Brand (BrandName)
import Web.ShoppingCart.Domain.Item (Item, ItemId, CreateItem, UpdateItem)


type Items m =
    { findAll :: m (List Item)
    , findBy :: BrandName -> m (List Item)
    , findById :: ItemId -> m (Maybe Item)
    , create :: CreateItem -> m Unit
    , update :: UpdateItem -> m Unit
    }
