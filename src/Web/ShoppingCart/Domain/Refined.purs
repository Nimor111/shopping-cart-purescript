module Web.ShoppingCart.Domain.Refined where

import Data.HeytingAlgebra (not)
import Data.Refinery.Core (class Validate, EvalTree(..))
import Data.String.Common (null)

data NonEmptyString

instance validateNonEmptyString :: Validate NonEmptyString String where
  validate _ val =
    { result: not (null val)
    , evalTree: Satisfy "nonEmpty"
    }
