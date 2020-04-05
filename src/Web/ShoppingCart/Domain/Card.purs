module Web.ShoppingCart.Domain.Card
        ( Card (..)
        , CardName (..)
        , CardNumber (..)
        , CardExpiration (..)
        , CVV (..)
        ) where


newtype CardName = CardName { unCardName :: String }
newtype CardNumber = CardNumber { unCardNumber :: String }
newtype CardExpiration = CardExpiration { unCardExpiration :: String }
newtype CVV = CVV { unCvv :: String }

type Card =
  { cardName :: CardName
  , cardNumber :: CardNumber
  , cardExpiration :: CardExpiration
  , cardCvv :: CVV
  }
