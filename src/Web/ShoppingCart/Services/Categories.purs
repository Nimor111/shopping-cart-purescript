module Web.ShoppingCart.Services.Categories
        ( class Categories
        , findAll
        , create
        ) where

import Prelude

import Data.List.Types (List)
import Web.ShoppingCart.Domain.Category (Category)


class Categories m where
    findAll :: m (List Category)
    create :: Category -> m Unit
