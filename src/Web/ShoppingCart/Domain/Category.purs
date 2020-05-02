module Web.ShoppingCart.Domain.Category
  ( Category(..)
  , CategoryId(..)
  , CategoryName(..)
  , RefinedCategoryDTO(..)
  ) where

import Prelude
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..))
import Data.List (singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Refinery.Core (Refined, refine, unrefine)
import Data.Show (show)
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeImpl)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Refined (NonEmptyString, ValidUUID, refineMaybe, mapMaybeToError, mapToError)
import Web.ShoppingCart.Domain.RefinedPred (NamePred(..), UUIDPred(..))

newtype CategoryId
  = CategoryId String

newtype CategoryName
  = CategoryName String

type CategoryDTO
  = { id :: Maybe String
    , name :: String
    }

data RefinedCategoryDTO
  = RefinedCategoryDTO (Maybe UUIDPred) NamePred

instance writeForeignRefinedCategory :: WriteForeign RefinedCategoryDTO where
  writeImpl (RefinedCategoryDTO uuid name) = writeImpl { id: map (\id -> unrefine $ unwrap id) uuid, name: unrefine $ unwrap name }

instance readForeignRefinedCategory :: ReadForeign RefinedCategoryDTO where
  readImpl val = do
    (r :: CategoryDTO) <- except $ read val
    refinedId <- except $ mapMaybeToError $ refineMaybe r.id
    refinedName <- except $ mapToError $ refine r.name
    except $ Right $ RefinedCategoryDTO (map UUIDPred refinedId) (NamePred refinedName)

derive instance newtypeCategoryId :: Newtype CategoryId _

derive instance newtypeCategoryName :: Newtype CategoryName _

derive newtype instance readForeignCategoryId :: JSON.ReadForeign CategoryId

derive newtype instance writeForeignCategoryId :: JSON.WriteForeign CategoryId

derive newtype instance readForeignCategoryName :: JSON.ReadForeign CategoryName

derive newtype instance writeForeignCategoryName :: JSON.WriteForeign CategoryName

type Category
  = { id :: CategoryId
    , name :: CategoryName
    }
