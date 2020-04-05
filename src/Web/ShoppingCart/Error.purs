module Web.ShoppingCart.Error
    ( handleGenericError
    )
    where

import Prelude

import Data.Variant (Variant, SProxy(..), on, case_)
import Data.Either (Either, either)
import HTTPure.Response (Response, ResponseM) as HTTPure
import HTTPure.Response (internalServerError)
import Database.PostgreSQL (PGError)

import Web.ShoppingCart.App (_pgError)


handleGenericError
    :: Either (Variant ( pgError :: PGError, error :: String )) HTTPure.Response
    -> HTTPure.ResponseM
handleGenericError = either genericError pure

genericError :: Variant (pgError :: PGError, error :: String) -> HTTPure.ResponseM
genericError =
    case_
      # on _pgError
        (\s -> internalServerError ("[PGERROR]: " <> show s))
      # on (SProxy :: SProxy "error")
        (\s -> internalServerError ("[ERROR]: " <> s))
