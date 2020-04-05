module Web.ShoppingCart.Services.Brands
        ( class Brands
        , findAll
        , create
        ) where

import Prelude

import Data.List.Types (List)
import Web.ShoppingCart.Domain.Brand (Brand)


class Brands m where
    findAll :: m (List Brand)
    create :: Brand -> m Unit
