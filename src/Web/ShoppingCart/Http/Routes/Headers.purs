module Web.ShoppingCart.Http.Routes.Headers
        ( responseHeaders
        ) where

import HTTPure.Headers (Headers, header) as HTTPure


responseHeaders :: HTTPure.Headers
responseHeaders = HTTPure.header "Content-Type" "application/json"
