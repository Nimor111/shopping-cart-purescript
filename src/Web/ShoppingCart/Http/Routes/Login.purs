module Web.ShoppingCart.Http.Routes.Login where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Newtype (un, unwrap, wrap)
import Data.Traversable (sequence)
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff.Class (class MonadAff)
import HTTPure ((!@), (!?))
import HTTPure.Body (class Body)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, created, noContent', notFound, ok, ok') as HTTPure
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.User (JwtToken(..), LoginUser(..))
import Web.ShoppingCart.Error (type (+), JsonDecodeError, LoginError, jsonDecodeError, loginError)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Auth (Auth)

type HandleLoginError r
  = Variant (LoginError + JsonDecodeError + r)

loginRouter ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadError (HandleLoginError r) m =>
  Auth m ->
  HTTPure.Request ->
  m HTTPure.Response
loginRouter authClient req@{ path: [ "login" ], method: Post, body } = do
  res <- handleLogin authClient body req
  case res of
    Left err -> throwError err
    Right v -> HTTPure.ok' responseHeaders (stringify $ encodeJson v)

loginRouter _ _ = HTTPure.notFound

handleLogin ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadError (HandleLoginError r) m =>
  Auth m ->
  String ->
  HTTPure.Request ->
  m (Either (HandleLoginError r) JwtToken)
handleLogin authClient body req =
  runExceptT
    $ do
        user <- ExceptT $ pure $ mapJsonError body
        ExceptT $ sequence $ Right (authClient.login user.userName user.password)
  where
  mapJsonError :: forall r1. String -> Either (Variant (JsonDecodeError + r1)) LoginUser
  mapJsonError body = case decodeJson =<< jsonParser body of
    Left error -> Left $ jsonDecodeError error
    Right v -> Right v
