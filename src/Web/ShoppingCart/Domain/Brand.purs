module Web.ShoppingCart.Domain.Brand
  ( Brand(..)
  , BrandId(..)
  , BrandName(..)
  , BrandNamePred(..)
  , toDomain
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
import Data.Newtype (class Newtype, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Refinery.Core (Refined, refine, unrefine)
import Data.Show (class Show)
import Foreign (ForeignError(..), readString)
import Foreign.Object (empty)
import Simple.JSON (class ReadForeign, class WriteForeign, read, readImpl, readJSON, writeImpl)
import Web.ShoppingCart.Domain.Refined (NonEmptyString)

newtype BrandNamePred
  = BrandNamePred (Refined NonEmptyString String)

newtype BrandId
  = BrandId String

newtype BrandName
  = BrandName String

derive newtype instance showBrandId :: Show BrandId

derive newtype instance showBrandName :: Show BrandName

instance writeForeignBrandNamePred :: WriteForeign BrandNamePred where
  writeImpl (BrandNamePred ref) = writeImpl (unrefine ref)

type BrandDTO
  = { id :: Maybe String
    , name :: String
    }

instance readForeignBrandNamePred :: ReadForeign BrandNamePred where
  readImpl val = case read val of
    Right (r :: BrandDTO) -> case refine r.name of
      Left err -> except $ Left $ NonEmptyList $ (ForeignError $ show err.evalTree) :| singleton (ForeignError err.value)
      Right v2 -> except $ Right (BrandNamePred v2)
    Left err -> except $ Left err

toDomain :: BrandNamePred -> BrandName
toDomain (BrandNamePred ref) = wrap $ unrefine ref

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
