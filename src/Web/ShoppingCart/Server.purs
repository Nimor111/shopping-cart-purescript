module Web.ShoppingCart.Server
  ( server
  , Services(..)
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import HTTPure (ServerM, serve) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, ResponseM) as HTTPure
import Web.ShoppingCart.App (AppError, App, runApp)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Error (handleRequestError)
import Web.ShoppingCart.Http.Middlewares.Auth (authMiddleware)
import Web.ShoppingCart.Http.Routes.Brands (brandsRouter)
import Web.ShoppingCart.Http.Routes.Admin.Brands as BrandAdmin
import Web.ShoppingCart.Router (Route, errorOut, route, router, sayHello)
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
  MonadError (AppError r) m =>
  Brands m ->
  Array (Route m)
appRoutes b =
  [ route [ "hello" ] sayHello
  , route [ "error" ] errorOut
  , route [ "brands" ] (brandsRouter b)
  , route [ "admin", "brands" ] (BrandAdmin.brandsRouter b)
  ]

authRoutes ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadError (AppError r) m =>
  Brands m ->
  Array (Route m)
authRoutes b =
  [ route [ "error" ] errorOut
  ]

type Services m
  = { brands :: Brands m
    {--, items :: Items m--}
    {--, auth :: Auth m--}
    {--, categories :: Categories m--}
    {--, orders :: Orders m--}
    {--, payments :: Payments m--}
    {--, users :: Users m--}
    {--, shoppingCart :: ShoppingCart m--}
    }

server :: forall r. Context -> Services (App r) -> HTTPure.ServerM
server ctx services = HTTPure.serve 8080 middlewares $ Console.log "Server up on port 8080"
  where
  middlewares = appMiddleware ctx (router $ appRoutes services.brands)
