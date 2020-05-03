module Web.ShoppingCart.Domain.RefinedPred
  ( NamePred(..)
  , UUIDPred(..)
  , PositiveNumberPred(..)
  , nameToDomain
  , numToDomain
  , uuidToDomain
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
import Data.Refinery.Predicate.Numeric (Pos)
import Foreign (ForeignError(..), readInt, readNumber, readString)
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

numToDomain :: forall a. Newtype a Int => PositiveNumberPred -> a
numToDomain (PositiveNumberPred name) = wrap $ unrefine name

uuidToDomain :: forall a. Newtype a String => UUIDPred -> a
uuidToDomain (UUIDPred uuid) = wrap $ unrefine uuid

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

newtype PositiveNumberPred
  = PositiveNumberPred (Refined Pos Int)

derive instance newtypePositiveNumberPred :: Newtype PositiveNumberPred _

instance readForeignPositiveNumberPred :: ReadForeign PositiveNumberPred where
  readImpl val = case runExcept $ readInt val of
    Left err -> except (Left err)
    Right v1 -> case refine v1 of
      Left err -> except $ Left $ NonEmptyList $ (ForeignError $ show err.evalTree) :| singleton (ForeignError $ show err.value)
      Right v2 -> except $ Right (PositiveNumberPred v2)

instance writeForeignPositiveNumberPred :: WriteForeign PositiveNumberPred where
  writeImpl (PositiveNumberPred ref) = writeImpl (unrefine ref)
