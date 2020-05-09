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
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (except)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:?), (.:))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=?), (~>), (:=), (~>?))
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Refinery.Core (Refined, refine, unrefine)
import Data.Refinery.Predicate.Numeric (Pos)
import Data.Show (class Show)
import Web.ShoppingCart.Domain.Brand (Brand, BrandId(..))
import Web.ShoppingCart.Domain.Category (Category, CategoryId(..))
import Web.ShoppingCart.Domain.Refined (NonEmptyString, ValidUUID, mapToError, refineIdentity, refineMaybe)
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

decodeItemDTO :: Json -> Either String ItemDTO
decodeItemDTO json = do
  obj <- decodeJson json
  id <- obj .:? "id"
  name <- obj .: "name"
  description <- obj .: "description"
  price <- obj .: "price"
  brandId <- obj .: "brandId"
  categoryId <- obj .: "categoryId"
  pure $ { id, name, description, price, brandId, categoryId }

data RefinedItemDTO
  = RefinedItemDTO (Maybe UUIDPred) NamePred NamePred PositiveNumberPred NamePred NamePred

instance encodeJsonRefinedItem :: EncodeJson RefinedItemDTO where
  encodeJson (RefinedItemDTO uuid name description price brandId categoryId) =
    "name" := (unrefine $ unwrap name)
      ~> "id"
      :=? map (\id -> unrefine $ unwrap id) uuid
      ~>? "description"
      := ( unrefine
            $ unwrap description
        )
      ~> "price"
      := ( unrefine
            $ unwrap price
        )
      ~> "brandId"
      := ( unrefine
            $ unwrap brandId
        )
      ~> "categoryId"
      := ( unrefine
            $ unwrap categoryId
        )
      ~> jsonEmptyObject

instance decodeJsonRefinedItem :: DecodeJson RefinedItemDTO where
  decodeJson json =
    runExcept
      $ do
          r <- except $ decodeItemDTO json
          refinedId <- except $ mapToError $ refineMaybe r.id
          refinedName <- except $ mapToError $ refineIdentity r.name
          refinedDescription <- except $ mapToError $ refineIdentity r.description
          refinedPrice <- except $ mapToError $ refineIdentity r.price
          refinedBrandId <- except $ mapToError $ refineIdentity r.brandId
          refinedCategoryId <- except $ mapToError $ refineIdentity r.categoryId
          except $ Right $ RefinedItemDTO (map UUIDPred refinedId) (NamePred $ unwrap refinedName) (NamePred $ unwrap refinedDescription) (PositiveNumberPred $ unwrap refinedPrice) (NamePred $ unwrap refinedBrandId) (NamePred $ unwrap refinedCategoryId)

type UpdateItemDTO
  = { id :: String
    , price :: Int
    }

data RefinedItemUpdateDTO
  = RefinedItemUpdateDTO UUIDPred PositiveNumberPred

instance encodeJsonRefinedItemUpdate :: EncodeJson RefinedItemUpdateDTO where
  encodeJson (RefinedItemUpdateDTO uuid price) =
    "price" := (unrefine $ unwrap price)
      ~> "id"
      := (unrefine $ unwrap uuid)
      ~> jsonEmptyObject

instance decodeJsonRefinedItemUpdate :: DecodeJson RefinedItemUpdateDTO where
  decodeJson json =
    runExcept
      $ do
          (r :: UpdateItemDTO) <- except $ decodeJson json
          refinedId <- except $ mapToError $ refineIdentity r.id
          refinedPrice <- except $ mapToError $ refineIdentity r.price
          except $ Right $ RefinedItemUpdateDTO (UUIDPred $ unwrap refinedId) (PositiveNumberPred $ unwrap refinedPrice)

derive instance newtypeItemId :: Newtype ItemId _

derive instance newtypeItemName :: Newtype ItemName _

derive instance newtypeItemDescription :: Newtype ItemDescription _

derive instance newtypeMoney :: Newtype Money _

derive newtype instance decodeJsonItemId :: DecodeJson ItemId

derive newtype instance encodeJsonItemId :: EncodeJson ItemId

derive newtype instance encodeJsonItemName :: EncodeJson ItemName

derive newtype instance decodeJsonItemName :: DecodeJson ItemName

derive newtype instance encodeJsonItemDescription :: EncodeJson ItemDescription

derive newtype instance decodeJsonItemDescription :: DecodeJson ItemDescription

derive newtype instance encodeJsonMoney :: EncodeJson Money

derive newtype instance decodeJsonMoney :: DecodeJson Money

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
