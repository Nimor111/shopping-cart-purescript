module Main where

import Prelude
import Effect (Effect)
import Web.ShoppingCart (runServer) as ShoppingCart
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Server (Services)
import Web.ShoppingCart.Services.Brands (mkBrands)

services :: forall r. Services (App r)
services =
  { brands: mkBrands
  }

main :: Effect Unit
main = ShoppingCart.runServer services
