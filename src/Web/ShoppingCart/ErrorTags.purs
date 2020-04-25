module Web.ShoppingCart.ErrorTags
    ( _orderNotFound
    , _databaseError
    , _jsonDecodeError
    , _unknownError
    ) where

import Data.Variant (SProxy(..))


_orderNotFound = SProxy :: SProxy "orderNotFoundError"
_databaseError = SProxy :: SProxy "databaseError"
_jsonDecodeError = SProxy :: SProxy "jsonDecodeError"
_unknownError = SProxy :: SProxy "unknownError"
