module Web.ShoppingCart.Server
        ( server
        )
        where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import HTTPure (ServerM, serve) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, ResponseM) as HTTPure
import Web.ShoppingCart.App (App, AppError, runApp)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Error (handleGenericError)
import Web.ShoppingCart.Http.Middlewares.Auth (authMiddleware)
import Web.ShoppingCart.Router (Route, router, route, errorOut, insertPeople, sayHello)
import Web.ShoppingCart.Services.Brands (Brands)


appMiddleware
   :: forall r
   .  Context
   -> (HTTPure.Request -> App r HTTPure.Response)
   -> HTTPure.Request
   -> HTTPure.ResponseM
appMiddleware ctx handler request =
    runApp ctx (handler request) >>= handleGenericError

appRoutes
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow (AppError r) m
    {--=> Brands m--}
    => Array (Route m)
appRoutes =
    [ route ["hello"] sayHello
    , route ["error"] (authMiddleware errorOut)
    , route ["insert"] insertPeople
    {--, route ["brands"] (brandsRouter b)--}
    ]

authRoutes
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow (AppError r) m
    {--=> Brands m--}
    => Array (Route m)
authRoutes =
    [ route ["insert"] insertPeople
    ]

type Services m =
    { brands :: Brands m
    }

-- TODO this will be the real server
{--server :: Context -> Services Aff -> HTTPure.ServerM--}
{--server services ctx = HTTPure.serve 8080 (appMiddleware ctx (router (appRoutes services))) $ Console.log "Server up on port 8080"--}

server :: Context -> HTTPure.ServerM
server ctx = HTTPure.serve 8080 middlewares $ Console.log "Server up on port 8080"
    where
        middlewares = appMiddleware ctx (router appRoutes)
