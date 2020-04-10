module Web.ShoppingCart.Http.Middlewares.Auth
        ( authMiddleware
        ) where

import Prelude

import Control.Monad (liftM1)
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import HTTPure.Headers (Headers(..))
import HTTPure.Lookup ((!!))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (notFound, badRequest, Response, ResponseM) as HTTPure
import Node.Simple.Jwt (JwtError(..), decode, fromString)
import Simple.JSON (class ReadForeign)
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Context (Context)


authMiddleware
    :: Context -- going to take secret from this eventually
    -> (HTTPure.Request -> Aff HTTPure.Response)
    -> HTTPure.Request
    -> HTTPure.ResponseM
authMiddleware ctx handler request = (liftEffect $ getAndVerifyAuth request) >>= \verified ->
    case verified of
         Right _  -> handler request
         Left  err  -> HTTPure.badRequest $ show err

lookupHeaderEither :: Headers -> Either JwtError String
lookupHeaderEither headers =
    case headers !! "Authorization" of
      Just t -> Right t
      Nothing -> Left InvalidTokenError

getAndVerifyAuth
    :: HTTPure.Request
    -> Effect (Either JwtError String)
getAndVerifyAuth { headers } = runExceptT $ do
    token <- ExceptT $ pure $ lookupHeaderEither headers

    ExceptT $ decode "tOpSeCrEt" (fromString token)
