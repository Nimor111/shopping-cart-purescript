module Web.ShoppingCart.Services.Brands
        ( Brands (..)
        ) where

import Prelude

import Data.List.Types (List)
import Web.ShoppingCart.Domain.Brand (Brand)


data Brands m = Brands
    { findAll :: m (List Brand)
    , create :: Brand -> m Unit
    }
