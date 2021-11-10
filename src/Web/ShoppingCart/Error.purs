module Web.ShoppingCart.Error
  ( handleRequestError
  , RequestError(..)
  , RowApply(..)
  , type (+)
  , OrderNotFoundError(..)
  , OrderCreateFailedError(..)
  , LoginError(..)
  , StringRefineError(..)
  , JwtTokenMissingError(..)
  , PaymentFailedError(..)
  , DatabaseError(..)
  , ShoppingCartJsonDecodeError(..)
  , UnknownError(..)
  , UserNameInUseError(..)
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
  ) where

import Prelude
import Data.Either (Either, either)
import Data.Refinery.Core (EvalTree)
import Data.Variant (Variant, inj, onMatch)
import Database.PostgreSQL (PGError)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error, message)
import HTTPure.Response (Response) as HTTPure
import HTTPure.Response (badRequest, conflict, forbidden, internalServerError, notFound)
import Web.ShoppingCart.ErrorTags (_databaseError, _jsonDecodeError, _orderNotFound, _unknownError, _orderCreateFailedError, _paymentFailedError, _loginError, _jwtTokenMissingError, _userNameInUseError, _stringRefineError)

type RowApply (f :: Row Type -> Row Type) (a :: Row Type)
  = f a

infixr 0 type RowApply as +

type ShoppingCartJsonDecodeError r
  = ( jsonDecodeError :: String | r )

jsonDecodeError :: forall r. String -> Variant (ShoppingCartJsonDecodeError + r)
jsonDecodeError = inj _jsonDecodeError

type OrderNotFoundError r
  = ( orderNotFoundError :: String | r )

orderNotFoundError :: forall r. String -> Variant (OrderNotFoundError + r)
orderNotFoundError = inj _orderNotFound

type DatabaseError r
  = ( databaseError :: PGError | r )

databaseError :: forall r. PGError -> Variant (DatabaseError + r)
databaseError = inj _databaseError

type UnknownError r
  = ( unknownError :: Error | r )

unknownError :: forall r. Error -> Variant (UnknownError + r)
unknownError = inj _unknownError

type OrderCreateFailedError r
  = ( orderCreateFailedError :: Unit | r )

orderCreateFailedError :: forall r. Variant (OrderCreateFailedError + r)
orderCreateFailedError = inj _orderCreateFailedError unit

type PaymentFailedError r
  = ( paymentFailedError :: { userId :: String, total :: Int } | r )

paymentFailedError :: forall r. String -> Int -> Variant (PaymentFailedError + r)
paymentFailedError userId total = inj _paymentFailedError { userId, total }

type LoginError r
  = ( loginError :: Unit | r )

loginError :: forall r. Variant (LoginError + r)
loginError = inj _loginError unit

type JwtTokenMissingError r
  = ( jwtTokenMissingError :: Unit | r )

jwtTokenMissingError :: forall r. Variant (JwtTokenMissingError + r)
jwtTokenMissingError = inj _jwtTokenMissingError unit

type UserNameInUseError r
  = ( userNameInUseError :: String | r )

userNameInUseError :: forall r. String -> Variant (UserNameInUseError + r)
userNameInUseError = inj _userNameInUseError

type StringRefineError r
  = ( stringRefineError :: { value :: String, evalTree :: EvalTree } | r )

stringRefineError :: forall r. { value :: String, evalTree :: EvalTree } -> Variant (StringRefineError + r)
stringRefineError = inj _stringRefineError

type RequestError r
  = ( ShoppingCartJsonDecodeError
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

handleRequestError ::
  forall m r.
  MonadAff m =>
  Either (Variant (RequestError + r)) HTTPure.Response ->
  m HTTPure.Response
handleRequestError = either handle pure

handle ::
  forall m r.
  MonadAff m =>
  Variant (RequestError + r) ->
  m HTTPure.Response
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
    , stringRefineError: \s -> badRequest ("[REFINE-ERROR] value " <> show s.value <> " should be: " <> show s.evalTree)
    }
    (\_ -> internalServerError "[ERROR]: Unknown")
