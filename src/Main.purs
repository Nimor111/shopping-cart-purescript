module Main where

import Prelude
import Effect (Effect)
import Web.ShoppingCart (runServer) as ShoppingCart

main :: Effect Unit
main = ShoppingCart.runServer
