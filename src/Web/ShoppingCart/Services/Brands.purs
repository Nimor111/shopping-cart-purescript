module Web.ShoppingCart.Services.Brands
        ( Brands (..)
        ) where

import Prelude

import Web.ShoppingCart.Domain.Brand (Brand)


type Brands m =
    { findAll :: m (Array Brand)
    , create :: Brand -> m Unit
    }
