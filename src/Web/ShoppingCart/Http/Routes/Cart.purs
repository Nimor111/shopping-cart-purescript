module Web.ShoppingCart.Http.Routes.Cart where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Logger.Class (class MonadLogger, info)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List.Types (NonEmptyList(..))
import Data.Map.Internal (empty)
import Data.Newtype (un, unwrap, wrap)
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Unit (Unit)
import Data.Variant (SProxy(..), Variant, inj)
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
import Web.ShoppingCart.Error (JsonDecodeError, jsonDecodeError, type (+))
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)

cartRouter ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  ShoppingCart m ->
  HTTPure.Request ->
  m HTTPure.Response
cartRouter cart req@{ path, method: Get } = getCartByUser (wrap $ path !@ 0) cart req

cartRouter cart req@{ path, method: Post, body } = do
  res <- addItemsToCart (wrap $ path !@ 0) body cart req -- take user id from session in the future?
  case res of
    Left err -> throwError $ jsonDecodeError err
    Right v -> HTTPure.ok ""

cartRouter cart req@{ path, method: Put, body } = do
  res <- updateCart (wrap $ path !@ 0) body cart req
  case res of
    Left err -> throwError $ jsonDecodeError err
    Right v -> HTTPure.ok ""

cartRouter cart req@{ path, method: Delete } = removeItemFromCart (wrap $ path !@ 0) (wrap $ path !@ 1) cart req

cartRouter _ _ = HTTPure.notFound

getCartByUser ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  UserId ->
  ShoppingCart m ->
  HTTPure.Request ->
  m HTTPure.Response
getCartByUser userId cart req = do
  info empty $ "Fetching cart for user with id" <> unwrap userId
  cartTotal <- cart.get userId
  HTTPure.ok' responseHeaders (JSON.writeJSON cartTotal)

addItemsToCart ::
  forall r m.
  MonadAff m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  MonadLogger m =>
  UserId ->
  String ->
  ShoppingCart m ->
  HTTPure.Request ->
  m (Either (NonEmptyList ForeignError) Unit)
addItemsToCart userId body cart req =
  runExceptT
    $ do
        (newCart :: Cart) <- ExceptT $ pure $ JSON.readJSON body
        info empty $ "Adding item to cart for user" <> show userId <> ". Updated cart: " <> show newCart
        ExceptT $ sequence $ Right (addItems newCart)
  where
  addItems :: Cart -> m Unit
  addItems newCart = traverse_ (\item -> cart.add userId item.itemId item.quantity) (unwrap newCart)

updateCart ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  UserId ->
  String ->
  ShoppingCart m ->
  HTTPure.Request ->
  m (Either (NonEmptyList ForeignError) Unit)
updateCart userId body cart req =
  runExceptT
    $ do
        (existingCart :: Cart) <- ExceptT $ pure $ JSON.readJSON body
        info empty $ "Updating cart for user" <> show userId <> ". Existing cart: " <> show existingCart
        ExceptT $ sequence $ Right (cart.update userId existingCart)

removeItemFromCart ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadThrow (Variant (JsonDecodeError + r)) m =>
  UserId ->
  ItemId ->
  ShoppingCart m ->
  HTTPure.Request ->
  m HTTPure.Response
removeItemFromCart userId itemId cart req = do
  info empty $ "Removing item " <> show itemId <> "for user " <> show userId
  cart.removeItem userId itemId
  HTTPure.noContent' responseHeaders
