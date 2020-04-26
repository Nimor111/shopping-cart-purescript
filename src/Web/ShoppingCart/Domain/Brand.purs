module Web.ShoppingCart.Domain.Brand
        ( Brand (..)
        , BrandId (..)
        , BrandName (..)
        , BrandNamePred (..)
        ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..))
import Data.List (singleton)
import Data.List.Types (List(..))
import Data.List.NonEmpty (cons)
import Data.List.Types (NonEmptyList(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Refinery.Core (Refined, refine, unrefine)
import Foreign (ForeignError(..), readString)
import Foreign.Object (empty)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Refined (NonEmptyString)

newtype BrandNamePred = BrandNamePred (Refined NonEmptyString String)

newtype BrandId = BrandId String
newtype BrandName = BrandName BrandNamePred

instance writeForeignBrandNamePred :: WriteForeign BrandNamePred where
    writeImpl (BrandNamePred ref) = writeImpl (unrefine ref)

instance readForeignBrandNamePred :: ReadForeign BrandNamePred where
    readImpl val = case runExcept $ readString val of
        Left err -> except (Left err)
        Right v1 -> case refine v1 of
            Left err -> except $ Left $ NonEmptyList $ (ForeignError $ show err.evalTree) :| singleton (ForeignError err.value)
            Right v2 -> except $ Right (BrandNamePred v2)

derive instance newtypeBrandId :: Newtype BrandId _
derive instance newtypeBrandName :: Newtype BrandName _

derive newtype instance readForeignBrandId :: JSON.ReadForeign BrandId
derive newtype instance writeForeignBrandId :: JSON.WriteForeign BrandId

derive newtype instance readForeignBrandName :: JSON.ReadForeign BrandName
derive newtype instance writeForeignBrandName :: JSON.WriteForeign BrandName

type Brand =
    { brandId :: BrandId
    , brandName :: BrandName
    }
