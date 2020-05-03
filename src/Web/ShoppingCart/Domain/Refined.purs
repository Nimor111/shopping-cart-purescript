module Web.ShoppingCart.Domain.Refined where

import Prelude
import Data.Either (Either(..))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..))
import Data.List.Types (NonEmptyList(..))
import Data.List (singleton)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Refinery.Core (class Validate, Error, EvalTree(..), Refined, refine)
import Foreign (ForeignError(..), readString)
import Data.String.Common (null)
import Data.UUID (parseUUID)

data NonEmptyString

instance validateNonEmptyString :: Validate NonEmptyString String where
  validate _ val =
    { result: not (null val)
    , evalTree: Satisfy "The string must be non empty!"
    }

data ValidUUID

instance validateUUID :: Validate ValidUUID String where
  validate _ val =
    { result:
        case parseUUID val of
          Just _ -> true
          Nothing -> false
    , evalTree: Satisfy "The UUID must be valid!"
    }

refineMaybe :: forall p a. Validate p a => Maybe a -> Either (Error a) (Maybe (Refined p a))
refineMaybe maybeValue = case maybeValue of
  Nothing -> Right Nothing
  Just v -> case refine v of
    Left err -> Left err
    Right v1 -> Right $ Just v1

mapMaybeToError :: forall p a. Show a => Either (Error a) (Maybe (Refined p a)) -> Either (NonEmptyList ForeignError) (Maybe (Refined p a))
mapMaybeToError refined = case refined of
  Right v -> Right v
  Left err -> Left $ NonEmptyList $ (ForeignError $ show err.evalTree) :| singleton (ForeignError $ show err.value)

mapToError :: forall p a. Show a => Either (Error a) (Refined p a) -> Either (NonEmptyList ForeignError) (Refined p a)
mapToError refined = case refined of
  Right v -> Right v
  Left err -> Left $ NonEmptyList $ (ForeignError $ show err.evalTree) :| singleton (ForeignError $ show err.value)
