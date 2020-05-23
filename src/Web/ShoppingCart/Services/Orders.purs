module Web.ShoppingCart.Services.Orders
  ( Orders(..)
  , mkOrders
  ) where

import Prelude

import Control.Bind (join)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Logger.Class (debug)
import Control.Monad.Reader.Trans (ReaderT(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (head)
import Data.Either (Either(..), either, hush)
import Data.Function (flip)
import Data.Identity (Identity(..))
import Data.Map.Internal (empty)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Database.PostgreSQL (PGError(..))
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Selda (leftJoin, restrict, selectFrom, (.==))
import Selda.Col (lit)
import Selda.PG.Class (insert1_, insert_, query)
import Selda.Query.Utils (RecordToArrayForeign(..))
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database (generateSQLStringFromQuery, hoistSelda)
import Web.ShoppingCart.Database.Tables (orders)
import Web.ShoppingCart.Domain.Item (ItemId(..), Money(..))
import Web.ShoppingCart.Domain.Order (OrderId(..), OrderItem, PaymentId(..), Order)
import Web.ShoppingCart.Domain.ShoppingCart (CartItem, Quantity(..))
import Web.ShoppingCart.Domain.User (UserId(..))
import Web.ShoppingCart.Error (databaseError)

type Orders m
  = { get :: UserId -> OrderId -> m (Maybe Order)
    , findBy :: UserId -> m (Array Order)
    , create :: OrderId -> UserId -> PaymentId -> Array CartItem -> Money -> m OrderId
    }

type DBOrderItem
  = { itemId :: String
    , quantity :: Int
    }

toOrderItem :: DBOrderItem -> OrderItem
toOrderItem { itemId, quantity } = { itemId: ItemId itemId, quantity: Quantity quantity }

type DBOrder
  = { id :: String
    , paymentId :: String
    , userId :: String
    , items :: Json
    , total :: Int
    }

mkOrders :: forall r. Orders (App r)
mkOrders =
  { get
  , findBy
  , create
  }

toOrder :: Array OrderItem -> DBOrder -> Order
toOrder decodedItems { id, paymentId, userId, items, total } = { id: OrderId id, paymentId: PaymentId paymentId, userId: UserId userId, items: decodedItems, total: Money total }

get :: forall r. UserId -> OrderId -> App r (Maybe Order)
get (UserId uid) (OrderId oid) = do
  let
    str = generateSQLStringFromQuery
    orderSql =
      selectFrom orders \{ id, paymentId, userId, items, total } -> do
        restrict $ userId .== (lit uid)
        restrict $ id .== (lit oid)
        pure $ { id, paymentId, userId, items, total }

  debug empty $ str orderSql
  hoistSelda do
    dbOrders <- query orderSql
    pure $ hush =<< map (\dbOrder ->
        case decodeJson dbOrder.items of
          Left err -> throwError (databaseError $ ConversionError err)
          Right items -> Right $ toOrder items dbOrder)
        (head dbOrders)

findBy :: forall r. UserId -> App r (Array Order)
findBy (UserId uid) = do
  let
    str = generateSQLStringFromQuery
    sql =
      selectFrom orders \{ id, paymentId, userId, items, total } -> do
        restrict $ userId .== (lit uid)
        pure { id, paymentId, userId, items, total }

  debug empty $ str sql
  hoistSelda do
    dbOrders <- query sql
    either throwError pure $ sequence $ map (\dbOrder ->
        case decodeJson dbOrder.items of
          Left err -> Left (ConversionError err)
          Right items -> Right $ toOrder items dbOrder)
        dbOrders

create :: forall r. OrderId -> UserId -> PaymentId -> Array CartItem -> Money -> App r OrderId
create orderId userId paymentId items total = do
  let
    str = generateSQLStringFromQuery
    dbOrderItems = encodeJson $ map (\cartItem -> { itemId: unwrap cartItem.item.id, quantity: unwrap cartItem.quantity }) items
    orderData = { id: unwrap orderId, paymentId: unwrap paymentId, userId: unwrap userId, items: dbOrderItems, total: unwrap total }

  debug empty $ "Creating new order..."
  hoistSelda do
    insert1_ orders orderData
    pure orderId
