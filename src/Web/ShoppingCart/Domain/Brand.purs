module Web.ShoppingCart.Domain.Brand
        ( Brand (..)
        , BrandId (..)
        , BrandName (..)
        ) where

import Prelude

import Data.Newtype (class Newtype)
import Simple.JSON as JSON


newtype BrandId = BrandId String
newtype BrandName = BrandName String

derive instance newtypeBrandId :: Newtype BrandId _
derive instance newtypeBrandName :: Newtype BrandName _

derive newtype instance readForeignBrandId :: JSON.ReadForeign BrandId
derive newtype instance writeForeignBrandId :: JSON.WriteForeign BrandId

derive newtype instance readForeignBrandName :: JSON.ReadForeign BrandName
derive newtype instance writeForeignBrandName :: JSON.WriteForeign BrandName

type Brand =
    { brandId :: BrandId
    , brandName :: BrandName
    }
