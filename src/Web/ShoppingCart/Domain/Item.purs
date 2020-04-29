module Web.ShoppingCart.Domain.Item
  ( Item(..)
  , ItemId(..)
  , ItemName(..)
  , ItemDescription(..)
  , CreateItem(..)
  , UpdateItem(..)
  , Money(..)
  ) where

import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Brand (Brand, BrandId)
import Web.ShoppingCart.Domain.Category (Category, CategoryId)

newtype ItemId
  = ItemId String

newtype ItemName
  = ItemName String

newtype ItemDescription
  = ItemDescription String

newtype Money
  = Money Number

derive newtype instance showMoney :: Show Money

derive instance newtypeItemId :: Newtype ItemId _

derive instance newtypeItemName :: Newtype ItemName _

derive instance newtypeItemDescription :: Newtype ItemDescription _

derive instance newtypeMoney :: Newtype Money _

derive newtype instance readForeignItemId :: JSON.ReadForeign ItemId

derive newtype instance writeForeignItemId :: JSON.WriteForeign ItemId

derive newtype instance readForeignItemName :: JSON.ReadForeign ItemName

derive newtype instance writeForeignItemName :: JSON.WriteForeign ItemName

derive newtype instance readForeignItemDescription :: JSON.ReadForeign ItemDescription

derive newtype instance writeForeignItemDescription :: JSON.WriteForeign ItemDescription

derive newtype instance readForeignMoney :: JSON.ReadForeign Money

derive newtype instance writeForeignMoney :: JSON.WriteForeign Money

type Item
  = { itemId :: ItemId
    , itemName :: ItemName
    , itemDescription :: ItemDescription
    , itemPrice :: Money
    , itemBrand :: Brand
    , itemCategory :: Category
    }

type CreateItem
  = { createItemName :: ItemName
    , createItemDescription :: ItemDescription
    , createItemPrice :: Money
    , createItemBrandId :: BrandId
    , createItemCategoryId :: CategoryId
    }

type UpdateItem
  = { updateItemId :: ItemId
    , updateItemPrice :: Money
    }
