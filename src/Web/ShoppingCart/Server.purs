module Web.ShoppingCart.Server
  ( server
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import HTTPure (ServerM, serve) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, ResponseM) as HTTPure
import Web.ShoppingCart.App (App, AppError, runApp)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Error (handleRequestError)
import Web.ShoppingCart.Http.Middlewares.Auth (authMiddleware)
import Web.ShoppingCart.Router (Route, router, route, errorOut, insertPeople, sayHello)
import Web.ShoppingCart.Services.Auth (Auth)
import Web.ShoppingCart.Services.Brands (Brands)
import Web.ShoppingCart.Services.Categories (Categories)
import Web.ShoppingCart.Services.Items (Items)
import Web.ShoppingCart.Services.Orders (Orders)
import Web.ShoppingCart.Services.Payments (Payments)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)
import Web.ShoppingCart.Services.Users (Users)

appMiddleware ::
  forall r.
  Context ->
  (HTTPure.Request -> App r HTTPure.Response) ->
  HTTPure.Request ->
  HTTPure.ResponseM
appMiddleware ctx handler request = runApp ctx (handler request) >>= handleRequestError

appRoutes ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadError (AppError r) m {--=> Brands m--}
  =>
  Array (Route m)
appRoutes =
  [ route [ "hello" ] sayHello
  , route [ "error" ] errorOut
  , route [ "insert" ] insertPeople
  {--, route ["brands"] (brandsRouter b)--}
  ]

authRoutes ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m {--=> Brands m--}
  =>
  Array (Route m)
authRoutes =
  [ route [ "insert" ] insertPeople
  ]

type Services m
  = { brands :: Brands m
    , items :: Items m
    , auth :: Auth m
    , categories :: Categories m
    , orders :: Orders m
    , payments :: Payments m
    , users :: Users m
    , shoppingCart :: ShoppingCart m
    }

-- TODO this will be the real server
{--server :: Context -> Services Aff -> HTTPure.ServerM--}
{--server services ctx = HTTPure.serve 8080 (appMiddleware ctx (router (appRoutes services))) $ Console.log "Server up on port 8080"--}
server :: Context -> HTTPure.ServerM
server ctx = HTTPure.serve 8080 middlewares $ Console.log "Server up on port 8080"
  where
  middlewares = appMiddleware ctx (router appRoutes)
