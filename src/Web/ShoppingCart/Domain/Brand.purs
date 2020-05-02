module Web.ShoppingCart.Domain.Brand
  ( Brand(..)
  , BrandId(..)
  , BrandName(..)
  , RefinedBrandDTO(..)
  ) where

import Prelude
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..))
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
import Simple.JSON (class ReadForeign, class WriteForeign, read, readImpl, readJSON, writeImpl)
import Web.ShoppingCart.Domain.Refined (NonEmptyString, ValidUUID, refineMaybe, mapMaybeToError, mapToError)
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

data RefinedBrandDTO
  = RefinedBrandDTO (Maybe UUIDPred) NamePred

instance writeForeignRefinedBrand :: WriteForeign RefinedBrandDTO where
  writeImpl (RefinedBrandDTO uuid name) = writeImpl { id: map (\id -> unrefine $ unwrap id) uuid, name: unrefine $ unwrap name }

instance readForeignRefinedBrand :: ReadForeign RefinedBrandDTO where
  readImpl val = do
    (r :: BrandDTO) <- except $ read val
    refinedId <- except $ mapMaybeToError $ refineMaybe r.id
    refinedName <- except $ mapToError $ refine r.name
    except $ Right $ RefinedBrandDTO (map UUIDPred refinedId) (NamePred refinedName)

derive instance newtypeBrandId :: Newtype BrandId _

derive instance newtypeBrandName :: Newtype BrandName _

derive newtype instance readForeignBrandId :: ReadForeign BrandId

derive newtype instance writeForeignBrandId :: WriteForeign BrandId

derive newtype instance readForeignBrandName :: ReadForeign BrandName

derive newtype instance writeForeignBrandName :: WriteForeign BrandName

type Brand
  = { id :: BrandId
    , name :: BrandName
    }
