module Web.ShoppingCart.Http.Routes.Register where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, notFound, ok') as HTTPure
import Simple.JSON (readJSON, writeJSON) as JSON
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.User (JwtToken, LoginUser)
import Web.ShoppingCart.Error (type (+), JsonDecodeError, UserNameInUseError, jsonDecodeError)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Auth (Auth)

type HandleRegisterError r = Variant (UserNameInUseError + JsonDecodeError + r)

registerRouter
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadError (HandleRegisterError r) m
    => Auth m
    -> HTTPure.Request
    -> m HTTPure.Response
registerRouter authClient req@{ path: ["register"], method: Post, body } = do
    res <- handleRegister authClient body req

    case res of
        Left err -> throwError err
        Right v -> HTTPure.ok' responseHeaders (JSON.writeJSON v)
registerRouter _ _ = HTTPure.notFound

handleRegister
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadError (HandleRegisterError r) m
    => Auth m
    -> String
    -> HTTPure.Request
    -> m (Either (HandleRegisterError r) JwtToken)
handleRegister authClient body req = runExceptT $ do
    user <- ExceptT $ pure $ mapJsonError body
    ExceptT $ sequence $ Right (authClient.newUser user.userName user.password)

    where
        mapJsonError :: forall r1. String -> Either (Variant (JsonDecodeError + r1)) LoginUser
        mapJsonError b = case JSON.readJSON b of
            Left errors -> Left $ jsonDecodeError errors
            Right v -> Right v
