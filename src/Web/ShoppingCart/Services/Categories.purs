module Web.ShoppingCart.Services.Categories
        ( Categories (..)
        ) where

import Prelude

import Web.ShoppingCart.Domain.Category (Category)


type Categories m =
    { findAll :: m (Array Category)
    , create :: Category -> m Unit
    }
