module Web.ShoppingCart.Domain.Brand
        ( Brand (..)
        , BrandId (..)
        , BrandName (..)
        ) where

import Prelude

import Simple.JSON as JSON


newtype BrandId = BrandId { unBrandId :: String }
newtype BrandName = BrandName { unBrandName :: String }

derive newtype instance readForeignBrandId :: JSON.ReadForeign BrandId
derive newtype instance writeForeignBrandId :: JSON.WriteForeign BrandId

derive newtype instance readForeignBrandName :: JSON.ReadForeign BrandName
derive newtype instance writeForeignBrandName :: JSON.WriteForeign BrandName

type Brand =
    { brandId :: BrandId
    , brandName :: BrandName
    }
