module Web.ShoppingCart.Http.Routes.Checkout where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList(..))
import Data.Newtype (un, unwrap, wrap)
import Data.Traversable (sequence)
import Data.Variant (inj, SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Foreign (ForeignError(..))
import HTTPure ((!@), (!?))
import HTTPure.Body (class Body)
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, noContent', notFound, ok, ok') as HTTPure
import Simple.JSON (readJSON, writeJSON) as JSON
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Card (Card)
import Web.ShoppingCart.Domain.Order (OrderId(..))
import Web.ShoppingCart.Domain.User (UserId(..))
import Web.ShoppingCart.Effects.Background (class Background, schedule)
import Web.ShoppingCart.Error (jsonDecodeError)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Programs.Checkout (checkout)
import Web.ShoppingCart.Services.Orders (Orders)
import Web.ShoppingCart.Services.Payments (Payments)
import Web.ShoppingCart.Services.ShoppingCart (ShoppingCart)


checkoutRouter
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadError (AppError r) m
    => Background m
    => Payments m
    -> ShoppingCart m
    -> Orders m
    -> HTTPure.Request
    -> m HTTPure.Response
checkoutRouter payments cart orders req@{ path, method: Post, body } = do
    res <- handleCheckout payments cart orders (wrap $ path !@ 0) body req

    case res of
        Left err -> throwError $ jsonDecodeError err
        Right v -> HTTPure.ok ""
checkoutRouter _ _ _ _ = HTTPure.notFound

handleCheckout
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadError (AppError r) m
    => Background m
    => Payments m
    -> ShoppingCart m
    -> Orders m
    -> UserId
    -> String
    -> HTTPure.Request
    -> m (Either (NonEmptyList ForeignError) OrderId)
handleCheckout payments cart orders userId body req = runExceptT $ do
    (card :: Card) <- ExceptT $ pure $ JSON.readJSON body
    ExceptT $ sequence $ Right (checkout payments cart orders userId card)
