module Web.ShoppingCart.Error
    ( handleRequestError
    , RequestError (..)
    , RowApply (..), type (+)
    , OrderNotFoundError (..)
    , OrderCreateFailedError (..)
    , PaymentFailedError (..)
    , DatabaseError (..)
    , JsonDecodeError (..)
    , UnknownError (..)
    , databaseError
    , orderNotFoundError
    , jsonDecodeError
    , unknownError
    , orderCreateFailedError
    , paymentFailedError
    )
    where

import Prelude

import Control.Monad.Except.Checked (handleError)
import Data.Either (Either, either)
import Data.List.Types (NonEmptyList(..))
import Data.Unit (unit)
import Data.Variant (SProxy(..), Variant, case_, inj, on, onMatch)
import Database.PostgreSQL (PGError)
import Effect.Exception (Error, message)
import Foreign (ForeignError(..))
import HTTPure.Response (Response, ResponseM) as HTTPure
import HTTPure.Response (badRequest, internalServerError, notFound)
import Web.ShoppingCart.Domain.Payment (Payment)
import Web.ShoppingCart.ErrorTags (_databaseError, _jsonDecodeError, _orderNotFound, _unknownError, _orderCreateFailedError, _paymentFailedError)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)


type RowApply (f :: # Type -> # Type) (a :: # Type) = f a

infixr 0 type RowApply as +

type JsonDecodeError r = (jsonDecodeError :: (NonEmptyList ForeignError) | r)

jsonDecodeError :: forall r. (NonEmptyList ForeignError) -> Variant (JsonDecodeError + r)
jsonDecodeError = inj _jsonDecodeError

type OrderNotFoundError r = (orderNotFoundError :: String | r)

orderNotFoundError :: forall r. String -> Variant (OrderNotFoundError + r)
orderNotFoundError = inj _orderNotFound

type DatabaseError r = (databaseError :: PGError | r)

databaseError :: forall r. PGError -> Variant (DatabaseError + r)
databaseError = inj _databaseError

type UnknownError r = (unknownError :: Error | r)

unknownError :: forall r. Error -> Variant (UnknownError + r)
unknownError = inj _unknownError

type OrderCreateFailedError r = (orderCreateFailedError :: Unit | r)

orderCreateFailedError :: forall r. Variant (OrderCreateFailedError + r)
orderCreateFailedError = inj _orderCreateFailedError unit

type PaymentFailedError r = (paymentFailedError :: Payment | r)

paymentFailedError :: forall r. Payment -> Variant (PaymentFailedError + r)
paymentFailedError = inj _paymentFailedError

type RequestError r =
    ( JsonDecodeError
    + OrderNotFoundError
    + DatabaseError
    + UnknownError
    + OrderCreateFailedError
    + PaymentFailedError
    + r
    )

handleRequestError
    :: forall r
    .  Either (Variant (RequestError + r)) HTTPure.Response
    -> HTTPure.ResponseM
handleRequestError = either handle pure


handle :: forall r. Variant (RequestError + r) -> HTTPure.ResponseM
handle =
    onMatch
      { databaseError: \s -> internalServerError ("[PGERROR]: " <> show s)
      , unknownError: \s -> internalServerError ("[ERROR]: " <> message s)
      , orderNotFoundError: (\_ -> notFound)
      , jsonDecodeError: \s -> badRequest ("[ERROR]: Bad request " <> show s)
      , orderCreateFailedError: \s -> badRequest ("[ERROR]: Failed to create order! ")
      , paymentFailedError: \s -> badRequest ("[ERROR]: Failed to process payment " <> show s)
      }
    (\_ -> internalServerError "[ERROR]: Unknown")
