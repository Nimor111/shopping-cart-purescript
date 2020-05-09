module Web.ShoppingCart.Domain.Brand
  ( Brand(..)
  , BrandId(..)
  , BrandName(..)
  , RefinedBrandDTO(..)
  ) where

import Prelude
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (except)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:?), (.:))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((:=?), (~>), (:=), (~>?))
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.List (singleton)
import Data.List.NonEmpty (cons)
import Data.List.Types (List(..))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Refinery.Core (Refined, Error, refine, unrefine)
import Data.Show (class Show)
import Foreign (ForeignError(..), readString)
import Foreign.Object (empty)
import Web.ShoppingCart.Domain.Refined (NonEmptyString, ValidUUID, mapToError, refineIdentity, refineMaybe)
import Web.ShoppingCart.Domain.RefinedPred (UUIDPred(..), NamePred(..))

newtype BrandId
  = BrandId String

newtype BrandName
  = BrandName String

derive newtype instance showBrandId :: Show BrandId

derive newtype instance showBrandName :: Show BrandName

type BrandDTO
  = { id :: Maybe String
    , name :: String
    }

decodeBrandDTO :: Json -> Either String BrandDTO
decodeBrandDTO json = do
  obj <- decodeJson json
  id <- obj .:? "id"
  name <- obj .: "name"
  pure $ { id, name }

data RefinedBrandDTO
  = RefinedBrandDTO (Maybe UUIDPred) NamePred

instance encodeJsonRefinedBrand :: EncodeJson RefinedBrandDTO where
  encodeJson (RefinedBrandDTO uuid name) =
    "name" := (unrefine $ unwrap name)
      ~> "id"
      :=? map (\id -> unrefine $ unwrap id) uuid
      ~>? jsonEmptyObject

instance decodeJsonRefinedBrand :: DecodeJson RefinedBrandDTO where
  decodeJson json =
    runExcept
      $ do
          r <- except $ decodeBrandDTO json
          refinedId <- except $ mapToError $ refineMaybe r.id
          refinedName <- except $ mapToError $ refineIdentity r.name
          except $ Right $ RefinedBrandDTO (map UUIDPred refinedId) (NamePred $ unwrap refinedName)

derive instance newtypeBrandId :: Newtype BrandId _

derive instance newtypeBrandName :: Newtype BrandName _

derive newtype instance encodeJsonBrandId :: EncodeJson BrandId

derive newtype instance decodeJsonBrandId :: DecodeJson BrandId

derive newtype instance encodeJsonBrandName :: EncodeJson BrandName

derive newtype instance decodeJsonBrandName :: DecodeJson BrandName

type Brand
  = { id :: BrandId
    , name :: BrandName
    }
