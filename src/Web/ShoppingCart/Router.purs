module Web.ShoppingCart.Router
        ( router
        )
        where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), inj)
import Effect.Aff.Class (class MonadAff)
import HTTPure (Response, ok) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (notFound) as HTTPure
import Selda.PG.Class (insert_)

import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Database (hoistSelda, people)


router
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => HTTPure.Request
    -> m HTTPure.Response
router req@{ path: ["hello"] } = sayHello req
router req@{ path: ["error"] } = errorOut req
router req@{ path: ["insert"] } = insertPeople req
router _ = HTTPure.notFound

insertPeople
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => HTTPure.Request
    -> m HTTPure.Response
insertPeople _ =
    hoistSelda (insert_ people peopleData) >>=
    (\_ -> HTTPure.ok $ "Inserted.")
    where
        peopleData :: Array { id :: Int, name :: String, age :: Maybe Int }
        peopleData =
            [ { id: 1, name: "name1", age: Just 11 }
            , { id: 2, name: "name2", age: Just 22 }
            , { id: 3, name: "name3", age: Just 33 }
            , { id: 4, name: "name4", age: Just 44 }
            , { id: 5, name: "name5", age: Just 55 }
            ]

errorOut
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => HTTPure.Request
    -> m HTTPure.Response
errorOut _ = do
    other <- asks _.other

    throwError ((inj (SProxy :: SProxy "error")) "this is an error")

sayHello
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => HTTPure.Request
    -> m HTTPure.Response
sayHello _ = do
    other <- asks _.other

    HTTPure.ok $ "Hello, " <> other
