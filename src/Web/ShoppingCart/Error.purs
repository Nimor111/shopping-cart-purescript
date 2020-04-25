module Web.ShoppingCart.Error
    ( handleGenericError
    , RequestError (..)
    , RowApply (..), type (+)
    , OrderNotFoundError (..)
    , DatabaseError (..)
    , JsonDecodeError (..)
    , UnknownError (..)
    , databaseError
    , orderNotFoundError
    , jsonDecodeError
    , unknownError
    )
    where

import Prelude

import Control.Monad.Except.Checked (handleError)
import Data.Either (Either, either)
import Data.List.Types (NonEmptyList(..))
import Data.Variant (SProxy(..), Variant, case_, inj, on, onMatch)
import Database.PostgreSQL (PGError)
import Effect.Exception (Error, message)
import Foreign (ForeignError(..))
import HTTPure.Response (Response, ResponseM) as HTTPure
import HTTPure.Response (badRequest, internalServerError, notFound)
import Web.ShoppingCart.ErrorTags (_databaseError, _jsonDecodeError, _orderNotFound, _unknownError)
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

type RequestError r =
    ( JsonDecodeError
    + OrderNotFoundError
    + DatabaseError
    + UnknownError
    + r
    )

handleGenericError
    :: forall r
    .  Either (Variant (RequestError + r)) HTTPure.Response
    -> HTTPure.ResponseM
handleGenericError = either genericError pure


genericError :: forall r. Variant (RequestError + r) -> HTTPure.ResponseM
genericError =
    onMatch
      { databaseError: \s -> internalServerError ("[PGERROR]: " <> show s)
      , unknownError: \s -> internalServerError ("[ERROR]: " <> message s)
      , orderNotFoundError: (\_ -> notFound)
      , jsonDecodeError: \s -> badRequest ("[ERROR]: Bad request " <> show s)
      }
    (\_ -> internalServerError "[ERROR]: Unknown")
