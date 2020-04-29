module Web.ShoppingCart.Domain.Card
  ( Card(..)
  , CardName(..)
  , CardNumber(..)
  , CardExpiration(..)
  , CVV(..)
  ) where

import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Simple.JSON as JSON

newtype CardName
  = CardName String

newtype CardNumber
  = CardNumber String

newtype CardExpiration
  = CardExpiration String

newtype CVV
  = CVV String

derive newtype instance showCardNumber :: Show CardNumber

derive newtype instance showCardExpiration :: Show CardExpiration

derive newtype instance showCardName :: Show CardName

derive newtype instance showCVV :: Show CVV

derive instance newtypeCardName :: Newtype CardName _

derive instance newtypeCardNumber :: Newtype CardNumber _

derive instance newtypeCardExpiration :: Newtype CardExpiration _

derive instance newtypeCVV :: Newtype CVV _

derive newtype instance readForeignCardName :: JSON.ReadForeign CardName

derive newtype instance writeForeignCardName :: JSON.WriteForeign CardName

derive newtype instance readForeignCardNumber :: JSON.ReadForeign CardNumber

derive newtype instance writeForeignCardNumber :: JSON.WriteForeign CardNumber

derive newtype instance readForeignCardExpiration :: JSON.ReadForeign CardExpiration

derive newtype instance writeForeignCardExpiration :: JSON.WriteForeign CardExpiration

derive newtype instance readForeignCVV :: JSON.ReadForeign CVV

derive newtype instance writeForeignCVV :: JSON.WriteForeign CVV

type Card
  = { cardName :: CardName
    , cardNumber :: CardNumber
    , cardExpiration :: CardExpiration
    , cardCvv :: CVV
    }
