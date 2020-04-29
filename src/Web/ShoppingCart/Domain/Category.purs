module Web.ShoppingCart.Domain.Category
  ( Category(..)
  , CategoryId(..)
  , CategoryName(..)
  ) where

import Data.Newtype (class Newtype)
import Simple.JSON as JSON

newtype CategoryId
  = CategoryId String

newtype CategoryName
  = CategoryName String

derive instance newtypeCategoryId :: Newtype CategoryId _

derive instance newtypeCategoryName :: Newtype CategoryName _

derive newtype instance readForeignCategoryId :: JSON.ReadForeign CategoryId

derive newtype instance writeForeignCategoryId :: JSON.WriteForeign CategoryId

derive newtype instance readForeignCategoryName :: JSON.ReadForeign CategoryName

derive newtype instance writeForeignCategoryName :: JSON.WriteForeign CategoryName

type Category
  = { categoryId :: CategoryId
    , categoryName :: CategoryName
    }
