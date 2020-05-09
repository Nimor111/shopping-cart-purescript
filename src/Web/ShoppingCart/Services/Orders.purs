module Web.ShoppingCart.Services.Orders
  ( Orders(..)
  ) where

import Prelude
import Control.Monad.Logger.Class (debug)
import Data.Array (head)
import Data.Map.Internal (empty)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Selda (leftJoin, restrict, selectFrom, (.==))
import Selda.Col (lit)
import Selda.PG.Class (insert1_, insert_, query)
import Web.ShoppingCart.App (App)
import Web.ShoppingCart.Database (generateSQLStringFromQuery, hoistSelda)
import Web.ShoppingCart.Database.Tables (orders)
import Web.ShoppingCart.Domain.Item (ItemId(..), Money(..))
import Web.ShoppingCart.Domain.Order (OrderId(..), OrderItem, PaymentId(..), Order)
import Web.ShoppingCart.Domain.ShoppingCart (CartItem, Quantity(..))
import Web.ShoppingCart.Domain.User (UserId(..))

type Orders m
  = { get :: UserId -> OrderId -> m (Maybe Order)
    , findBy :: UserId -> m (Array Order)
    , create :: UserId -> PaymentId -> Array CartItem -> Money -> m OrderId
    }

{--type DBOrderItem--}
{--= { orderId :: String--}
{--, itemId :: String--}
{--, quantity :: Int--}
{--}
--}
{--toOrderItem :: DBOrderItem -> OrderItem--}
{--toOrderItem { itemId, quantity } = { itemId: ItemId itemId, quantity: Quantity quantity }--}
{--type DBOrder--}
{--= { id :: String--}
{--, paymentId :: String--}
{--, userId :: String--}
{--, total :: Int--}
{--}
--}
{--mkOrders :: forall r. Orders (App r)--}
{--mkOrders =--}
{--{ get--}
{--, findBy--}
{--, create--}
{--}
--}
{--toOrder :: Array DBOrderItem -> DBOrder -> Order--}
{--toOrder items { id, paymentId, userId, total } = { id: OrderId id, paymentId: PaymentId paymentId, userId: UserId userId, items: map toOrderItem items, total: Money total }--}
{--get :: forall r. UserId -> OrderId -> App r (Maybe Order)--}
{--get (UserId uid) (OrderId oid) = do--}
{--let--}
{--str = generateSQLStringFromQuery--}
{--orderSql =--}
{--selectFrom orders \{ id, paymentId, userId, total } -> do--}
{--restrict $ userId .== (lit uid)--}
{--restrict $ id .== (lit oid)--}
{--pure { id, paymentId, userId, total }--}
{--orderItemsSql =--}
{--selectFrom orderItems \{ itemId, orderId, quantity } -> do--}
{--restrict $ orderId .== (lit oid)--}
{--pure { itemId, quantity }--}
{--debug empty $ str orderSql--}
{--debug empty $ str orderItemsSql--}
{--hoistSelda do--}
{--dbOrders <- query orderSql--}
{--dbOrderItems <- query orderItemsSql--}
{--pure $ map (\dbOrder -> toOrder dbOrderItems dbOrder) $ head dbOrders--}
{---- TODO: This is becoming too complicated with the orderItems table, worth checking out how to do json stuff with postgres-client--}
{--findBy :: forall r. UserId -> App r (Array Order)--}
{--findBy (UserId uid) = do--}
{--let--}
{--str = generateSQLStringFromQuery--}
{--let--}
{--sql =--}
{--selectFrom orders \{ id, paymentId, userId, total } -> do--}
{--restrict $ userId .== (lit uid)--}
{--pure { id, paymentId, userId, total }--}
{--debug empty $ str sql--}
{--[>hoistSelda do<]--}
{--[>dbOrders <- query sql<]--}
{--[>pure dbOrders<]--}
{--[>let<]--}
{--[>orderItemsSqls = map (\dbOrder -> orderItemSql dbOrder.id) dbOrders<]--}
{--[>dbOrderItems <- traverse (\itemSql -> query itemSql) orderItemsSqls<]--}
{--[>pure $ map (\dbOrder -> toOrder<]--}
{--where--}
{--orderItemSql oid =--}
{--selectFrom orderItems \{ itemId, orderId, quantity } -> do--}
{--restrict $ orderId .== (lit oid)--}
{--pure { itemId, quantity }--}
{--create :: forall r. Order -> App r Unit--}
{--create orderId userId paymentId items total = do--}
{--let--}
{--str = generateSQLStringFromQuery--}
{--let--}
{--orderData = { id: unwrap orderId, paymentId: unwrap paymentId, userId: unwrap userId, total: unwrap total }--}
{--orderItemsData = map (\item -> { itemId: item.id, orderId: unwrap orderId, quantity: unwrap item.quantity }) items--}
{--hoistSelda do--}
{--insert1_ orders orderData--}
{--insert_ orderItems orderItemsData--}
