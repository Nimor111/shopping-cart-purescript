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
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.List (singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Newtype (class Newtype, wrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Refinery.Core (class Validate, Refined, refine, unrefine)
import Data.Refinery.Predicate.Numeric (Pos)
import Foreign (readInt, readNumber, readString)
import Web.ShoppingCart.Domain.Refined (NonEmptyString, ValidUUID)

newtype NamePred
  = NamePred (Refined NonEmptyString String)

derive instance newtypeNamePred :: Newtype NamePred _

instance decodeJsonNamePred :: DecodeJson NamePred where
  decodeJson json = case decodeJson json of
    Left err -> Left $ TypeMismatch ("JSON decode error for value: " <> show err)
    Right v1 -> case refine v1 of
      Left err -> Left $ TypeMismatch ("Refine error. Value " <> show err.value <> " should be: " <> show err.evalTree)
      Right v2 -> Right (NamePred v2)

instance encodeJsonNamePred :: EncodeJson NamePred where
  encodeJson (NamePred ref) = encodeJson (unrefine ref)

nameToDomain :: forall a. Newtype a String => NamePred -> a
nameToDomain (NamePred name) = wrap $ unrefine name

numToDomain :: forall a. Newtype a Int => PositiveNumberPred -> a
numToDomain (PositiveNumberPred name) = wrap $ unrefine name

uuidToDomain :: forall a. Newtype a String => UUIDPred -> a
uuidToDomain (UUIDPred uuid) = wrap $ unrefine uuid

newtype UUIDPred
  = UUIDPred (Refined ValidUUID String)

derive instance newtypeUUIDPred :: Newtype UUIDPred _

instance decodeJsonUUIDPred :: DecodeJson UUIDPred where
  decodeJson json = case decodeJson json of
    Left err -> Left $ TypeMismatch ("JSON decode error for value: " <> show err)
    Right v1 -> case refine v1 of
      Left err -> Left $ TypeMismatch ("Refine error. Value " <> show err.value <> " should be: " <> show err.evalTree)
      Right v2 -> Right (UUIDPred v2)

instance encodeJsonUUIDPred :: EncodeJson UUIDPred where
  encodeJson (UUIDPred ref) = encodeJson (unrefine ref)

newtype PositiveNumberPred
  = PositiveNumberPred (Refined Pos Int)

derive instance newtypePositiveNumberPred :: Newtype PositiveNumberPred _

instance decodeJsonPositiveNumberPred :: DecodeJson PositiveNumberPred where
  decodeJson json = case decodeJson json of
    Left err -> Left $ TypeMismatch ("JSON decode error for value: " <> show err)
    Right v1 -> case refine v1 of
      Left err -> Left $ TypeMismatch ("Refine error. Value " <> show err.value <> " should be: " <> show err.evalTree)
      Right v2 -> Right (PositiveNumberPred v2)

instance encodeJsonPositiveNumberPred :: EncodeJson PositiveNumberPred where
  encodeJson (PositiveNumberPred ref) = encodeJson (unrefine ref)
