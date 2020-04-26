module Web.ShoppingCart.Http.Middlewares.Auth
        ( authMiddleware
        ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import HTTPure.Headers (Headers(..))
import HTTPure.Lookup ((!!))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (notFound, badRequest, Response, ResponseM, forbidden) as HTTPure
import Node.Simple.Jwt (JwtError(..), decode, fromString, Secret)
import Web.ShoppingCart.App (App, AppError, runApp)
import Web.ShoppingCart.Context (Context)


authMiddleware
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => (HTTPure.Request -> m HTTPure.Response)
    -> HTTPure.Request
    -> m HTTPure.Response
authMiddleware handler request =
    authenticate handler request

authenticate
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => (HTTPure.Request -> m HTTPure.Response)
    -> HTTPure.Request
    -> m HTTPure.Response
authenticate handler request = do
    secret <- asks _.jwtSecret
    verified <- liftEffect $ getAndVerifyAuth secret request
    case verified of
        Right _  -> handler request
        Left  err  -> HTTPure.forbidden

lookupHeaderEither :: Headers -> Either JwtError String
lookupHeaderEither headers =
    case headers !! "Authorization" of
      Just t -> Right t
      Nothing -> Left InvalidTokenError

getAndVerifyAuth
    :: forall m
    .  MonadEffect m
    => Secret
    -> HTTPure.Request
    -> m (Either JwtError String)
getAndVerifyAuth secret { headers } = runExceptT $ do
    token <- ExceptT $ pure $ lookupHeaderEither headers

    ExceptT $ liftEffect $ decode secret (fromString token)
