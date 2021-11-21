module Web.ShoppingCart.App
  ( App
  , AppError
  , runApp
  ) where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Logger.Trans (LoggerT(..), runLoggerT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Web.ShoppingCart.Context (Context)
import Web.ShoppingCart.Error (RequestError, type (+))

type AppError r
  = Variant (RequestError + r)

type App r
  = LoggerT (ReaderT Context (ExceptT (AppError r) Aff))

runApp :: forall a r. Context -> App r a -> Aff (Either (AppError r) a)
runApp ctx app = runExceptT $ runReaderT (runLoggerT app $ prettyFormatter >=> Console.log) ctx
