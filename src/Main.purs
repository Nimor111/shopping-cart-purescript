module Main where

import Prelude
import Effect (Effect)
import Web.ShoppingCart (runServer) as ShoppingCart
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Server (Services)
import Web.ShoppingCart.Services.Brands (mkBrands)
import Web.ShoppingCart.Services.Categories (mkCategories)
import Web.ShoppingCart.Services.Items (mkItems)
import Web.ShoppingCart.Services.Orders (mkOrders)

services :: forall r. Services (App r)
services =
  { brands: mkBrands
  , categories: mkCategories
  , items: mkItems
  , orders: mkOrders
  }

main :: Effect Unit
main = ShoppingCart.runServer services
