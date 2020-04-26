module Web.ShoppingCart.ErrorTags
    ( _orderNotFound
    , _databaseError
    , _jsonDecodeError
    , _unknownError
    , _orderCreateFailedError
    , _paymentFailedError
    , _loginError
    ) where

import Data.Variant (SProxy(..))


_orderNotFound = SProxy :: SProxy "orderNotFoundError"
_databaseError = SProxy :: SProxy "databaseError"
_jsonDecodeError = SProxy :: SProxy "jsonDecodeError"
_unknownError = SProxy :: SProxy "unknownError"
_orderCreateFailedError = SProxy :: SProxy "orderCreateFailedError"
_paymentFailedError = SProxy :: SProxy "paymentFailedError"
_loginError = SProxy :: SProxy "loginError"
