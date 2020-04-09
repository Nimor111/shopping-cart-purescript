module Web.ShoppingCart.Domain.Category
        ( Category (..)
        , CategoryId (..)
        , CategoryName (..)
        ) where

import Simple.JSON as JSON


newtype CategoryId = CategoryId { unCategoryId :: String }
newtype CategoryName = CategoryName { unCategoryName :: String }

derive newtype instance readForeignCategoryId :: JSON.ReadForeign CategoryId
derive newtype instance writeForeignCategoryId :: JSON.WriteForeign CategoryId

derive newtype instance readForeignCategoryName :: JSON.ReadForeign CategoryName
derive newtype instance writeForeignCategoryName :: JSON.WriteForeign CategoryName

type Category =
    { categoryId :: CategoryId
    , categoryName :: CategoryName
    }
