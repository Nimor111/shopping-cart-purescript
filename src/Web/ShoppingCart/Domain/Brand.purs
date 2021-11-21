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
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=?), (~>), (:=), (~>?))
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Refinery.Core (unrefine)
import Web.ShoppingCart.Domain.Refined (mapToJsonError, refineIdentity, refineMaybe)
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

decodeBrandDTO :: Json -> Either JsonDecodeError BrandDTO
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
          refinedId <- except $ mapToJsonError $ refineMaybe r.id
          refinedName <- except $ mapToJsonError $ refineIdentity r.name
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
