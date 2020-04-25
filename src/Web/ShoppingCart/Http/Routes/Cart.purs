module Web.ShoppingCart.Http.Routes.Cart where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List.Types (NonEmptyList(..))
import Data.Newtype (un, unwrap, wrap)
import Data.Traversable (sequence)
import Data.Unit (Unit)
import Data.Variant (inj, SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (ForeignError(..))
import HTTPure ((!@))
import HTTPure.Body (class Body)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, noContent', notFound, ok, ok') as HTTPure
import Simple.JSON (class ReadForeign)
import Simple.JSON as JSON
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Item (ItemId(..))
import Web.ShoppingCart.Domain.ShoppingCart (Cart(..))
import Web.ShoppingCart.Domain.User (UserId(..))
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)


cartRouter
    :: forall m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => ShoppingCart m
    -> HTTPure.Request
    -> m HTTPure.Response
cartRouter cart req@{ path, method: Get } = getCartByUser (wrap $ path !@ 0) cart req
cartRouter cart req@{ path, method: Post, body } = do
    res <- addItemsToCart (wrap $ path !@ 0) body cart req -- take user id from session in the future?
    case res of
        Left err -> throwError ((inj (SProxy :: SProxy "error")) (show err))
        Right v -> HTTPure.ok ""
cartRouter cart req@{ path, method: Put, body } = do
    res <- updateCart (wrap $ path !@ 0) body cart req

    case res of
        Left err -> throwError ((inj (SProxy :: SProxy "error")) (show err))
        Right v -> HTTPure.ok ""
cartRouter cart req@{ path, method: Delete } = removeItemFromCart (wrap $ path !@ 0) (wrap $ path !@ 1) cart req
cartRouter _ _ = HTTPure.notFound

getCartByUser
    :: forall m
    .  MonadAff m
    => MonadThrow AppError m
    => UserId
    -> ShoppingCart m
    -> HTTPure.Request
    -> m HTTPure.Response
getCartByUser userId cart req = do
    liftEffect $ log ("Fetching cart for user with id" <> unwrap userId)
    cartTotal <- cart.get userId

    HTTPure.ok' responseHeaders (JSON.writeJSON cartTotal)

addItemsToCart
    :: forall m
    .  MonadAff m
    => MonadThrow AppError m
    => UserId
    -> String
    -> ShoppingCart m
    -> HTTPure.Request
    -> m (Either (NonEmptyList ForeignError) Unit)
addItemsToCart userId body cart req = runExceptT $ do
    (newCart :: Cart) <- ExceptT $ pure $ JSON.readJSON body
    ExceptT $ sequence $ Right (addItems newCart)
    where
        addItems :: Cart -> m Unit
        addItems newCart = traverse_ (\item -> cart.add userId item.itemId item.quantity) (unwrap newCart)

updateCart
    :: forall m
    .  MonadAff m
    => MonadThrow AppError m
    => UserId
    -> String
    -> ShoppingCart m
    -> HTTPure.Request
    -> m (Either (NonEmptyList ForeignError) Unit)
updateCart userId body cart req = runExceptT $ do
    (existingCart :: Cart) <- ExceptT $ pure $ JSON.readJSON body
    ExceptT $ sequence $ Right (cart.update userId existingCart)

removeItemFromCart
    :: forall m
    .  MonadAff m
    => MonadThrow AppError m
    => UserId
    -> ItemId
    -> ShoppingCart m
    -> HTTPure.Request
    -> m HTTPure.Response
removeItemFromCart userId itemId cart req = do
    cart.removeItem userId itemId

    HTTPure.noContent' responseHeaders
