module Web.ShoppingCart.Services.Brands
        ( Brands (..)
        ) where

import Prelude

import Data.List.Types (List)
import Web.ShoppingCart.Domain.Brand (Brand)


type Brands m =
    { findAll :: m (List Brand)
    , create :: Brand -> m Unit
    }
