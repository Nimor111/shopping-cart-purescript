module Web.ShoppingCart.Domain.Card
  ( Card(..)
  , CardName(..)
  , CardNumber(..)
  , CardExpiration(..)
  , CVV(..)
  ) where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Newtype (class Newtype)
import Data.Show (class Show)

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

derive newtype instance decodeJsonCardName :: DecodeJson CardName

derive newtype instance encodeJsonCardName :: EncodeJson CardName

derive newtype instance decodeJsonCardNumber :: DecodeJson CardNumber

derive newtype instance encodeJsonCardNumber :: EncodeJson CardNumber

derive newtype instance decodeJsonCardExpiration :: DecodeJson CardExpiration

derive newtype instance encodeJsonCardExpiration :: EncodeJson CardExpiration

derive newtype instance decodeJsonCVV :: DecodeJson CVV

derive newtype instance encodeJsonCVV :: EncodeJson CVV

type Card
  = { cardName :: CardName
    , cardNumber :: CardNumber
    , cardExpiration :: CardExpiration
    , cardCvv :: CVV
    }
