module Web.ShoppingCart.Domain.Item
  ( Item(..)
  , ItemId(..)
  , ItemName(..)
  , ItemDescription(..)
  , RefinedItemDTO(..)
  , RefinedItemUpdateDTO(..)
  , CreateItem(..)
  , UpdateItem(..)
  , Money(..)
  ) where

import Prelude
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Refinery.Core (Refined, refine, unrefine)
import Data.Refinery.Predicate.Numeric (Pos)
import Data.Show (class Show)
import Foreign (readUndefined)
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeImpl)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Brand (Brand, BrandId(..))
import Web.ShoppingCart.Domain.Category (Category, CategoryId(..))
import Web.ShoppingCart.Domain.Refined (NonEmptyString, ValidUUID, mapMaybeToError, mapToError, refineMaybe)
import Web.ShoppingCart.Domain.RefinedPred (NamePred(..), PositiveNumberPred(..), UUIDPred(..))

newtype ItemId
  = ItemId String

newtype ItemName
  = ItemName String

newtype ItemDescription
  = ItemDescription String

newtype Money
  = Money Int

derive newtype instance showMoney :: Show Money

derive newtype instance showItemId :: Show ItemId

derive newtype instance showItemName :: Show ItemName

derive newtype instance showItemDescription :: Show ItemDescription

type ItemDTO
  = { id :: Maybe String
    , name :: String
    , description :: String
    , price :: Int
    , brandId :: String
    , categoryId :: String
    }

type UpdateItemDTO
  = { id :: String
    , price :: Int
    }

data RefinedItemDTO
  = RefinedItemDTO (Maybe UUIDPred) NamePred NamePred PositiveNumberPred NamePred NamePred

instance writeForeignRefinedItem :: WriteForeign RefinedItemDTO where
  writeImpl (RefinedItemDTO uuid name description price brandId categoryId) =
    writeImpl
      { id: map (\id -> unrefine $ unwrap id) uuid
      , name: unrefine $ unwrap name
      , description: unrefine $ unwrap description
      , price: unrefine $ unwrap price
      , brandId: unrefine $ unwrap brandId
      , categoryId: unrefine $ unwrap categoryId
      }

instance readForeignRefinedItem :: ReadForeign RefinedItemDTO where
  readImpl val = do
    (r :: ItemDTO) <- except $ read val
    refinedId <- except $ mapMaybeToError $ refineMaybe r.id
    refinedName <- except $ mapToError $ refine r.name
    refinedDescription <- except $ mapToError $ refine r.description
    refinedPrice <- except $ mapToError $ refine r.price
    refinedBrandId <- except $ mapToError $ refine r.brandId
    refinedCategoryId <- except $ mapToError $ refine r.categoryId
    except $ Right $ RefinedItemDTO (map UUIDPred refinedId) (NamePred refinedName) (NamePred refinedDescription) (PositiveNumberPred refinedPrice) (NamePred refinedBrandId) (NamePred refinedCategoryId)

data RefinedItemUpdateDTO
  = RefinedItemUpdateDTO UUIDPred PositiveNumberPred

instance writeForeignRefinedUpdateItem :: WriteForeign RefinedItemUpdateDTO where
  writeImpl (RefinedItemUpdateDTO uuid price) =
    writeImpl
      { id: unrefine $ unwrap uuid
      , price: unrefine $ unwrap price
      }

instance readForeignRefinedUpdateItem :: ReadForeign RefinedItemUpdateDTO where
  readImpl val = do
    (r :: UpdateItemDTO) <- except $ read val
    refinedId <- except $ mapToError $ refine r.id
    refinedPrice <- except $ mapToError $ refine r.price
    except $ Right $ RefinedItemUpdateDTO (UUIDPred refinedId) (PositiveNumberPred refinedPrice)

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
  = { id :: ItemId
    , name :: ItemName
    , description :: ItemDescription
    , price :: Money
    , brandId :: BrandId
    , categoryId :: CategoryId
    }

type CreateItem
  = { id :: ItemId
    , name :: ItemName
    , description :: ItemDescription
    , price :: Money
    , brandId :: BrandId
    , categoryId :: CategoryId
    }

type UpdateItem
  = { id :: ItemId
    , price :: Money
    }
