module Web.ShoppingCart.Http.Routes.Items where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.Refinery.Core (EvalTree, refine)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure ((!@))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, notFound, ok') as HTTPure
import Simple.JSON as JSON
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Brand (BrandName(..), BrandNamePred(..))
import Web.ShoppingCart.Domain.Item (Item)
import Web.ShoppingCart.Error (stringRefineError)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Items (Items)


itemsRouter
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow (AppError r) m
    => Items m
    -> HTTPure.Request
    -> m HTTPure.Response
itemsRouter items req@{ path: [""] } = do
    res <- getItemsByBrandName items req

    case res of
        Left err -> throwError err
        Right items -> HTTPure.ok' responseHeaders (JSON.writeJSON items)
itemsRouter _ _ = HTTPure.notFound

getItemsByBrandName
    :: forall r m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow (AppError r) m
    => Items m
    -> HTTPure.Request
    -> m (Either (AppError r) (Array Item))
getItemsByBrandName i req = runExceptT $ do
    -- liftEffect $ log "Fetching all items by brand name..."
    refinedBrandName <- ExceptT $ pure $ mapRefinedToError (req.query !@ "brand")
    ExceptT $ sequence $ Right $ i.findBy (wrap refinedBrandName)

    where
        mapRefinedToError :: String -> Either (AppError r) BrandNamePred
        mapRefinedToError brandName = case refine brandName of
            Left err -> Left $ stringRefineError err
            Right v -> Right $ BrandNamePred v
