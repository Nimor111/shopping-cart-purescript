module Web.ShoppingCart.Domain.Item
        ( Item(..)
        , ItemId(..)
        , ItemName (..)
        , ItemDescription (..)
        , CreateItem (..)
        , UpdateItem (..)
        , Money (..)
        ) where


import Data.UUID (UUID)
import Web.ShoppingCart.Domain.Brand (Brand, BrandId)
import Web.ShoppingCart.Domain.Category (Category, CategoryId)


newtype ItemId = ItemId { unItemId :: UUID }
newtype ItemName = ItemName { unItemName :: String }
newtype ItemDescription = ItemDescription { unItemDescription :: String }
newtype Money = Money { unMoney :: Number }

data Item = Item
    { itemId :: ItemId
    , itemName :: ItemName
    , itemDescription :: ItemDescription
    , itemPrice :: Money
    , itemBrand :: Brand
    , itemCategory :: Category
    }

data CreateItem = CreateItem
    { createItemName :: ItemName
    , createItemDescription :: ItemDescription
    , createItemPrice :: Money
    , createItemBrandId :: BrandId
    , createItemCategoryId :: CategoryId
    }

data UpdateItem = UpdateItem
    { updateItemId :: ItemId
    , updateItemPrice :: Money
    }
