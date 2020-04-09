module Web.ShoppingCart.Domain.Card
        ( Card (..)
        , CardName (..)
        , CardNumber (..)
        , CardExpiration (..)
        , CVV (..)
        ) where

import Simple.JSON as JSON

newtype CardName = CardName { unCardName :: String }
newtype CardNumber = CardNumber { unCardNumber :: String }
newtype CardExpiration = CardExpiration { unCardExpiration :: String }
newtype CVV = CVV { unCvv :: String }

derive newtype instance readForeignCardName :: JSON.ReadForeign CardName
derive newtype instance writeForeignCardName :: JSON.WriteForeign CardName

derive newtype instance readForeignCardNumber :: JSON.ReadForeign CardNumber
derive newtype instance writeForeignCardNumber :: JSON.WriteForeign CardNumber

derive newtype instance readForeignCardExpiration :: JSON.ReadForeign CardExpiration
derive newtype instance writeForeignCardExpiration :: JSON.WriteForeign CardExpiration

derive newtype instance readForeignCVV :: JSON.ReadForeign CVV
derive newtype instance writeForeignCVV :: JSON.WriteForeign CVV

type Card =
  { cardName :: CardName
  , cardNumber :: CardNumber
  , cardExpiration :: CardExpiration
  , cardCvv :: CVV
  }
