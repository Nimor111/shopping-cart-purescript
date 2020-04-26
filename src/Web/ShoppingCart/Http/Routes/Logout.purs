module Web.ShoppingCart.Http.Routes.Logout where

import Prelude

import Data.Maybe (Maybe(..))
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.Newtype (un, unwrap, wrap)
import Data.Traversable (sequence)
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff.Class (class MonadAff)
import HTTPure ((!@), (!?))
import HTTPure.Body (class Body)
import HTTPure.Headers (Headers(..))
import HTTPure.Method (Method(..))
import HTTPure.Lookup ((!!))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, created, noContent, noContent', notFound, ok, ok') as HTTPure
import Simple.JSON (readJSON, writeJSON) as JSON
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.User (JwtToken(..), LoginUser(..))
import Web.ShoppingCart.Error (type (+), JwtTokenMissingError, jwtTokenMissingError)
import Web.ShoppingCart.Http.Routes.Checkout (HandleCheckoutError)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Auth (Auth)

type HandleLogoutError r = Variant (JwtTokenMissingError + r)

logoutRouter
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadError (HandleLogoutError r) m
    => Auth m
    -> HTTPure.Request
    -> m HTTPure.Response
logoutRouter authClient req@{ path: ["logout"], method: Post, headers } = do
    res <- handleLogout authClient headers req

    case res of
        Left err -> throwError err
        Right v -> HTTPure.noContent
logoutRouter _ _ = HTTPure.notFound

handleLogout
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadError (HandleLogoutError r) m
    => Auth m
    -> Headers
    -> HTTPure.Request
    -> m (Either (HandleLogoutError r) Unit)
handleLogout authClient headers req = runExceptT $ do
    token <- ExceptT $ pure $ mapHeaders headers
    ExceptT $ sequence $ Right (authClient.logout token)

    where
        mapHeaders :: forall r1. Headers -> Either (Variant (JwtTokenMissingError + r1)) JwtToken
        mapHeaders headers = case headers !! "Authorization" of
            Nothing -> Left jwtTokenMissingError
            Just v -> Right (wrap v)
