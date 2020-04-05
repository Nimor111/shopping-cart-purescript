module Web.ShoppingCart.Domain.Category
        ( Category (..)
        , CategoryId (..)
        , CategoryName (..)
        ) where

import Data.UUID (UUID)


newtype CategoryId = CategoryId { unCategoryId :: UUID }
newtype CategoryName = CategoryName { unCategoryName :: String }

data Category = Category
    { categoryId :: CategoryId
    , categoryName :: CategoryName
    }
