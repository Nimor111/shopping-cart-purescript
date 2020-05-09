module Web.ShoppingCart.Domain.Refined
  ( refineMaybe
  , refineIdentity
  , mapToError
  , ValidUUID(..)
  , NonEmptyString(..)
  ) where

import Prelude
import Data.Either (Either(..))
import Data.HeytingAlgebra (not)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Refinery.Core (class Validate, Error, EvalTree(..), Refined, refine)
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

refineIdentity :: forall p a. Validate p a => a -> Either (Error a) (Identity (Refined p a))
refineIdentity value = case refine value of
  Left err -> Left err
  Right v -> Right $ Identity v

mapToError :: forall p a f. Show a => Either (Error a) (f (Refined p a)) -> Either String (f (Refined p a))
mapToError refined = case refined of
  Right v -> Right v
  Left err -> Left $ "Refine error during decoding: value " <> show err.value <> " should be: " <> show err.evalTree
