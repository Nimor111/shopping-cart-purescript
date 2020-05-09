module Web.ShoppingCart.Http.Routes.Items where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Logger.Class (class MonadLogger, info)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Data.Map.Internal (empty)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Refinery.Core (refine)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure ((!@))
import HTTPure.Method (Method(..))
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (Response, notFound, ok') as HTTPure
import Web.ShoppingCart.App (AppError)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Domain.Brand (BrandName(..))
import Web.ShoppingCart.Domain.Item (Item)
import Web.ShoppingCart.Domain.RefinedPred (NamePred(..), UUIDPred(..), nameToDomain, uuidToDomain)
import Web.ShoppingCart.Error (stringRefineError)
import Web.ShoppingCart.Http.Routes.Headers (responseHeaders)
import Web.ShoppingCart.Services.Items (Items)

itemsRouter ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Items m ->
  HTTPure.Request ->
  m HTTPure.Response
itemsRouter items req@{ path: [], method: Get } = do
  res <- getItemsByBrandName items req
  case res of
    Left err -> throwError err
    Right is -> HTTPure.ok' responseHeaders (stringify $ encodeJson is)

itemsRouter items req@{ path: [ itemId ], method: Get } = do
  res <- getItemById items itemId
  case res of
    Left err -> throwError err
    Right item -> HTTPure.ok' responseHeaders (stringify $ encodeJson item)

itemsRouter _ _ = HTTPure.notFound

getItemById ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Items m ->
  String ->
  m (Either (AppError r) (Maybe Item))
getItemById items itemId =
  runExceptT
    $ do
        refinedItemId <- ExceptT $ pure $ mapRefinedToEither itemId
        info empty $ "Fetching item with id " <> itemId
        ExceptT $ sequence $ Right $ items.findById (uuidToDomain refinedItemId)
  where
  mapRefinedToEither :: String -> Either (AppError r) UUIDPred
  mapRefinedToEither id = case refine id of
    Left err -> Left $ stringRefineError err
    Right v -> Right $ UUIDPred v

getItemsByBrandName ::
  forall r m.
  MonadAff m =>
  MonadLogger m =>
  MonadAsk Context m =>
  MonadThrow (AppError r) m =>
  Items m ->
  HTTPure.Request ->
  m (Either (AppError r) (Array Item))
getItemsByBrandName i req =
  runExceptT
    $ do
        refinedBrandName <- ExceptT $ pure $ mapRefinedToEither (req.query !@ "brand")
        info empty $ "Fetching all items for brand " <> show (nameToDomain refinedBrandName :: BrandName)
        ExceptT $ sequence $ Right $ i.findBy (nameToDomain refinedBrandName)
  where
  mapRefinedToEither :: String -> Either (AppError r) NamePred
  mapRefinedToEither brandName = case refine brandName of
    Left err -> Left $ stringRefineError err
    Right v -> Right $ NamePred v
