module Web.ShoppingCart.App
        ( App
        , AppError
        , runApp
        )
        where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Variant (SProxy(..), Variant)
import Database.PostgreSQL (PGError)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Type.Equality (class TypeEquals, from)
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Error (RequestError, type (+))


type AppError r = Variant (RequestError + r)

type App r = ReaderT Context (ExceptT (AppError r) Aff)

runApp :: ∀ a r. Context -> App r a -> Aff (Either (AppError r) a)
runApp ctx m = runExceptT $ runReaderT m ctx

