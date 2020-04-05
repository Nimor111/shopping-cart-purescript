module Web.ShoppingCart.Server
        ( server
        )
        where

import Prelude

import Effect.Class.Console as Console
import HTTPure.Response (Response, ResponseM) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure (ServerM, serve) as HTTPure

import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.App (App, runApp)
import Web.ShoppingCart.Error (handleGenericError)
import Web.ShoppingCart.Router (router)


appMiddleware
   :: Context
   -> (HTTPure.Request -> App HTTPure.Response)
   -> HTTPure.Request
   -> HTTPure.ResponseM
appMiddleware ctx r request =
    runApp ctx (r request) >>= handleGenericError

server :: Context -> HTTPure.ServerM
server ctx = HTTPure.serve 8080 (appMiddleware ctx router) $ Console.log "Server up on port 8080"
