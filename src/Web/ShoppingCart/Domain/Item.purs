module Web.ShoppingCart.Domain.Item
        ( Item(..)
        , ItemId(..)
        , ItemName (..)
        , ItemDescription (..)
        , CreateItem (..)
        , UpdateItem (..)
        , Money (..)
        ) where

import Simple.JSON as JSON

import Web.ShoppingCart.Domain.Brand (Brand, BrandId)
import Web.ShoppingCart.Domain.Category (Category, CategoryId)


newtype ItemId = ItemId { unItemId :: String }
newtype ItemName = ItemName { unItemName :: String }
newtype ItemDescription = ItemDescription { unItemDescription :: String }
newtype Money = Money { unMoney :: Number }

derive newtype instance readForeignItemId :: JSON.ReadForeign ItemId
derive newtype instance writeForeignItemId :: JSON.WriteForeign ItemId

derive newtype instance readForeignItemName :: JSON.ReadForeign ItemName
derive newtype instance writeForeignItemName :: JSON.WriteForeign ItemName

derive newtype instance readForeignItemDescription :: JSON.ReadForeign ItemDescription
derive newtype instance writeForeignItemDescription :: JSON.WriteForeign ItemDescription

derive newtype instance readForeignMoney :: JSON.ReadForeign Money
derive newtype instance writeForeignMoney :: JSON.WriteForeign Money

type Item =
    { itemId :: ItemId
    , itemName :: ItemName
    , itemDescription :: ItemDescription
    , itemPrice :: Money
    , itemBrand :: Brand
    , itemCategory :: Category
    }

type CreateItem =
    { createItemName :: ItemName
    , createItemDescription :: ItemDescription
    , createItemPrice :: Money
    , createItemBrandId :: BrandId
    , createItemCategoryId :: CategoryId
    }

type UpdateItem =
    { updateItemId :: ItemId
    , updateItemPrice :: Money
    }
