module Web.ShoppingCart.Server
  ( server
  , Services(..)
  ) where

import Prelude

import Control.Bind ((>=>))
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Logger.Class (class MonadLogger, info)
import Control.Monad.Logger.Trans (runLoggerT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Map.Internal (empty)
import Effect.Aff (launchAff_, runAff, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import HTTPure (ServerM, serve) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, ResponseM) as HTTPure
import Web.ShoppingCart.App (AppError, App, runApp)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Database.Tables (categories)
import Web.ShoppingCart.Error (handleRequestError)
import Web.ShoppingCart.Http.Middlewares.Auth (authMiddleware)
import Web.ShoppingCart.Http.Routes.Admin.Brands as BrandAdmin
import Web.ShoppingCart.Http.Routes.Admin.Categories as CategoryAdmin
import Web.ShoppingCart.Http.Routes.Admin.Items as ItemAdmin
import Web.ShoppingCart.Http.Routes.Brands (brandsRouter)
import Web.ShoppingCart.Http.Routes.Categories (categoriesRouter)
import Web.ShoppingCart.Http.Routes.Items (itemsRouter)
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
  MonadLogger m =>
  MonadAsk Context m =>
  MonadError (AppError r) m =>
  Services m ->
  Array (Route m)
appRoutes services =
  [ route [ "hello" ] sayHello
  , route [ "error" ] errorOut
  , route [ "brands" ] (brandsRouter services.brands)
  , route [ "admin", "brands" ] (BrandAdmin.brandsRouter services.brands)
  , route [ "categories" ] (categoriesRouter services.categories)
  , route [ "admin", "categories" ] (CategoryAdmin.categoriesRouter services.categories)
  , route [ "items" ] (itemsRouter services.items)
  , route [ "admin", "items" ] (ItemAdmin.itemsRouter services.items)
  ]

authRoutes ::
  forall r m.
  MonadAff m =>
  MonadAsk Context m =>
  MonadError (AppError r) m =>
  Services m ->
  Array (Route m)
authRoutes services =
  [ route [ "error" ] errorOut
  ]

type Services m
  = { brands :: Brands m
    , items :: Items m
    , categories :: Categories m
    , orders :: Orders m
    {--, auth :: Auth m--}
    {--, payments :: Payments m--}
    {--, users :: Users m--}
    {--, shoppingCart :: ShoppingCart m--}
    }

server :: forall r. Context -> Services (App r) -> HTTPure.ServerM
server ctx services = HTTPure.serve 8080 middlewares callback
  where
  middlewares = appMiddleware ctx (router $ appRoutes services)
  callback = launchAff_ $ runApp ctx $ info empty $ "Starting server on port 8080..."
