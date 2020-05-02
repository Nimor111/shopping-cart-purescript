module Web.ShoppingCart.Domain.RefinedPred
  ( NamePred(..)
  , UUIDPred(..)
  , nameToDomain
  ) where

import Prelude
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..))
import Data.List (singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Newtype (class Newtype, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Refinery.Core (class Validate, Refined, refine, unrefine)
import Foreign (ForeignError(..), readString)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Simple.JSON as JSON
import Web.ShoppingCart.Domain.Refined (NonEmptyString, ValidUUID)

newtype NamePred
  = NamePred (Refined NonEmptyString String)

derive instance newtypeNamePred :: Newtype NamePred _

instance readForeignNamePred :: ReadForeign NamePred where
  readImpl val = case runExcept $ readString val of
    Left err -> except (Left err)
    Right v1 -> case refine v1 of
      Left err -> except $ Left $ NonEmptyList $ (ForeignError $ show err.evalTree) :| singleton (ForeignError err.value)
      Right v2 -> except $ Right (NamePred v2)

instance writeForeignNamePred :: WriteForeign NamePred where
  writeImpl (NamePred ref) = writeImpl (unrefine ref)

nameToDomain :: forall a. Newtype a String => NamePred -> a
nameToDomain (NamePred name) = wrap $ unrefine name

newtype UUIDPred
  = UUIDPred (Refined ValidUUID String)

derive instance newtypeUUIDPred :: Newtype UUIDPred _

instance readForeignUUIDPred :: ReadForeign UUIDPred where
  readImpl val = case runExcept $ readString val of
    Left err -> except (Left err)
    Right v1 -> case refine v1 of
      Left err -> except $ Left $ NonEmptyList $ (ForeignError $ show err.evalTree) :| singleton (ForeignError err.value)
      Right v2 -> except $ Right (UUIDPred v2)

instance writeForeignUUIDPred :: WriteForeign UUIDPred where
  writeImpl (UUIDPred ref) = writeImpl (unrefine ref)
