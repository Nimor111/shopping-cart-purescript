module Web.ShoppingCart.Domain.Category
  ( Category(..)
  , CategoryId(..)
  , CategoryName(..)
  , CategoryNamePred(..)
  , toDomain
  ) where

import Prelude
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe)
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..))
import Data.List (singleton)
import Data.Newtype (class Newtype, wrap)
import Data.Refinery.Core (Refined, refine, unrefine)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Show (show)
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, read, writeImpl)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Refined (NonEmptyString)

newtype CategoryNamePred
  = CategoryNamePred (Refined NonEmptyString String)

newtype CategoryId
  = CategoryId String

newtype CategoryName
  = CategoryName String

instance writeForeignCategoryNamePred :: WriteForeign CategoryNamePred where
  writeImpl (CategoryNamePred ref) = writeImpl (unrefine ref)

type CategoryDTO
  = { id :: Maybe String
    , name :: String
    }

instance readForeignCategoryNamePred :: ReadForeign CategoryNamePred where
  readImpl val = case read val of
    Right (r :: CategoryDTO) -> case refine r.name of
      Left err -> except $ Left $ NonEmptyList $ (ForeignError $ show err.evalTree) :| singleton (ForeignError err.value)
      Right v2 -> except $ Right (CategoryNamePred v2)
    Left err -> except $ Left err

toDomain :: CategoryNamePred -> CategoryName
toDomain (CategoryNamePred ref) = wrap $ unrefine ref

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
