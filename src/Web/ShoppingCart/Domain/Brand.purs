module Web.ShoppingCart.Domain.Brand
        ( Brand (..)
        , BrandId (..)
        , BrandName (..)
        ) where

import Data.UUID (UUID)


newtype BrandId = BrandId { unBrandId :: UUID }
newtype BrandName = BrandName { unBrandName :: String }

data Brand = Brand
    { brandId :: BrandId
    , brandName :: BrandName
    }
