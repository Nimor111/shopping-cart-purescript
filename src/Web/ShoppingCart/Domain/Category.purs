module Web.ShoppingCart.Domain.Category
  ( Category(..)
  , CategoryId(..)
  , CategoryName(..)
  , RefinedCategoryDTO(..)
  ) where

import Prelude
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (except)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode.Combinators ((.:?), (.:))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=?), (~>), (:=), (~>?))
import Data.Either (Either(..))
import Data.List (singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Refinery.Core (Refined, refine, unrefine)
import Data.Show (show)
import Web.ShoppingCart.Domain.Refined (NonEmptyString, ValidUUID, refineMaybe, refineIdentity, mapToError)
import Web.ShoppingCart.Domain.RefinedPred (NamePred(..), UUIDPred(..))

newtype CategoryId
  = CategoryId String

newtype CategoryName
  = CategoryName String

type CategoryDTO
  = { id :: Maybe String
    , name :: String
    }

decodeCategoryDTO :: Json -> Either String CategoryDTO
decodeCategoryDTO json = do
  obj <- decodeJson json
  id <- obj .:? "id"
  name <- obj .: "name"
  pure $ { id, name }

data RefinedCategoryDTO
  = RefinedCategoryDTO (Maybe UUIDPred) NamePred

instance encodeJsonRefinedCategory :: EncodeJson RefinedCategoryDTO where
  encodeJson (RefinedCategoryDTO uuid name) =
    "name" := (unrefine $ unwrap name)
      ~> "id"
      :=? map (\id -> unrefine $ unwrap id) uuid
      ~>? jsonEmptyObject

instance decodeJsonRefinedCategory :: DecodeJson RefinedCategoryDTO where
  decodeJson json =
    runExcept
      $ do
          r <- except $ decodeCategoryDTO json
          refinedId <- except $ mapToError $ refineMaybe r.id
          refinedName <- except $ mapToError $ refineIdentity r.name
          except $ Right $ RefinedCategoryDTO (map UUIDPred refinedId) (NamePred $ unwrap refinedName)

derive instance newtypeCategoryId :: Newtype CategoryId _

derive instance newtypeCategoryName :: Newtype CategoryName _

derive newtype instance encodeJsonCategoryId :: EncodeJson CategoryId

derive newtype instance decodeJsonCategoryId :: DecodeJson CategoryId

derive newtype instance encodeJsonCategoryName :: EncodeJson CategoryName

derive newtype instance decodeJsonCategoryName :: DecodeJson CategoryName

type Category
  = { id :: CategoryId
    , name :: CategoryName
    }
