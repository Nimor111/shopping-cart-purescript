module Web.ShoppingCart.Error
    ( handleRequestError
    , RequestError (..)
    , RowApply (..), type (+)
    , OrderNotFoundError (..)
    , OrderCreateFailedError (..)
    , LoginError (..)
    , StringRefineError (..)
    , JwtTokenMissingError (..)
    , PaymentFailedError (..)
    , DatabaseError (..)
    , JsonDecodeError (..)
    , UnknownError (..)
    , UserNameInUseError (..)
    , databaseError
    , orderNotFoundError
    , jsonDecodeError
    , unknownError
    , orderCreateFailedError
    , paymentFailedError
    , loginError
    , jwtTokenMissingError
    , userNameInUseError
    , stringRefineError
    )
    where

import Prelude

import Control.Monad.Except.Checked (handleError)
import Data.Either (Either, either)
import Data.List.Types (NonEmptyList(..))
import Data.Refinery.Core (EvalTree)
import Data.Show (class Show)
import Data.Unit (unit)
import Data.Variant (SProxy(..), Variant, case_, inj, on, onMatch)
import Database.PostgreSQL (PGError)
import Effect.Exception (Error, message)
import Foreign (ForeignError(..))
import HTTPure.Response (Response, ResponseM) as HTTPure
import HTTPure.Response (badRequest, conflict, forbidden, internalServerError, notFound)
import Web.ShoppingCart.Domain.Payment (Payment)
import Web.ShoppingCart.ErrorTags (_databaseError, _jsonDecodeError, _orderNotFound, _unknownError, _orderCreateFailedError, _paymentFailedError, _loginError, _jwtTokenMissingError, _userNameInUseError, _stringRefineError)
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

type LoginError r = (loginError :: Unit | r)

loginError :: forall r. Variant (LoginError + r)
loginError = inj _loginError unit

type JwtTokenMissingError r = (jwtTokenMissingError :: Unit | r)

jwtTokenMissingError :: forall r. Variant (JwtTokenMissingError + r)
jwtTokenMissingError = inj _jwtTokenMissingError unit

type UserNameInUseError r = (userNameInUseError :: String | r)

userNameInUseError :: forall r. String -> Variant (UserNameInUseError + r)
userNameInUseError = inj _userNameInUseError

type StringRefineError r = (stringRefineError :: { value :: String, evalTree :: EvalTree } | r)

stringRefineError :: forall r. { value :: String, evalTree :: EvalTree } -> Variant (StringRefineError + r)
stringRefineError = inj _stringRefineError

type RequestError r =
    ( JsonDecodeError
    + OrderNotFoundError
    + DatabaseError
    + UnknownError
    + OrderCreateFailedError
    + PaymentFailedError
    + LoginError
    + JwtTokenMissingError
    + UserNameInUseError
    + StringRefineError
    + r
    )

handleRequestError
    :: forall r
    .  Either (Variant (RequestError + r)) HTTPure.Response
    -> HTTPure.ResponseM
handleRequestError = either handle pure


handle
    :: forall r
    .  Variant (RequestError + r)
    -> HTTPure.ResponseM
handle =
    onMatch
      { databaseError: \s -> internalServerError ("[PGERROR]: " <> show s)
      , unknownError: \s -> internalServerError ("[ERROR]: " <> message s)
      , orderNotFoundError: (\_ -> notFound)
      , jsonDecodeError: \s -> badRequest ("[ERROR]: Bad request " <> show s)
      , orderCreateFailedError: \s -> badRequest ("[ERROR]: Failed to create order! ")
      , paymentFailedError: \s -> badRequest ("[ERROR]: Failed to process payment " <> show s)
      , loginError: \_ -> forbidden
      , jwtTokenMissingError: \_ -> badRequest ("[ERROR]: Jwt token missing in request")
      , userNameInUseError: \s -> conflict ("[ERROR]: User name in use " <> show s)
      , stringRefineError: \s -> badRequest ("[REFINE-ERROR] value" <> show s.value <> "should be: " <> show s.evalTree)
      }
    (\_ -> internalServerError "[ERROR]: Unknown")
