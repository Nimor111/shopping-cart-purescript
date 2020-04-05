module Web.ShoppingCart.Services.Categories
        ( Categories (..)
        ) where

import Prelude

import Data.List.Types (List)
import Web.ShoppingCart.Domain.Category (Category)


data Categories m = Categories
    { findAll :: m (List Category)
    , create :: Category -> m Unit
    }
