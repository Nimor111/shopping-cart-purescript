module Web.ShoppingCart.App
        ( App
        , AppError
        , runApp
        , _pgError
        )
        where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either)
import Data.Variant (SProxy(..), Variant)
import Database.PostgreSQL (PGError)
import Effect.Aff (Aff)

import Web.ShoppingCart.Context (Context)


type AppError = Variant
    ( pgError :: PGError
    , error :: String
    )

_pgError = SProxy :: SProxy "pgError"

type App = ReaderT Context (ExceptT AppError Aff)

runApp :: ∀ a. Context -> App a -> Aff (Either AppError a)
runApp ctx m = runExceptT $ runReaderT m ctx
