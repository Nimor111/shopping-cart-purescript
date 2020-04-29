module Web.ShoppingCart.Services.Brands
  ( Brands(..)
  ) where

import Prelude
import Web.ShoppingCart.Domain.Brand (Brand, BrandId(..), BrandName(..))

type Brands m
  = { findAll :: m (Array Brand)
    , create :: Brand -> m Unit
    }

type BrandDTO
  = { brandDTOId :: String
    , brandDTOName :: String
    }

toBrand :: BrandDTO -> Brand
toBrand { brandDTOId, brandDTOName } = { brandId: BrandId brandDTOId, brandName: BrandName brandDTOName }
