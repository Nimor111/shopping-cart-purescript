module Web.ShoppingCart.Retry where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), class Duration)
import Data.Time.Duration (fromDuration)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Retry (RetryPolicyM(..), RetryStatus(..), limitRetries, retryPolicy, RetryPolicy) as Retry
import Effect.Exception (Error)
import Math (pow)


retryPolicy
    :: forall m
    .  MonadAff m
    => Retry.RetryPolicyM m
retryPolicy = exponentialBackoff (Milliseconds 2000.0) <> Retry.limitRetries 3

checks
    :: forall m e
    .  MonadAff m
    => MonadError e m
    => Array (Retry.RetryStatus -> e -> m Boolean)
checks = [\(Retry.RetryStatus { iterNumber: n }) error -> pure true]

exponentialBackoff
    :: forall d
    .  Duration d
    => d
    -> Retry.RetryPolicy
exponentialBackoff base = Retry.retryPolicy \(Retry.RetryStatus { iterNumber: n }) ->
    Just $ Milliseconds $ unwrap (fromDuration base) * pow 2.0 (toNumber n)

type RetryError =
    { error :: Error
    }
