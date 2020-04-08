module Web.ShoppingCart.Effects.Background
        ( class Background
        , schedule
        ) where

import Prelude

import Data.Functor (void)
import Data.Time.Duration (Milliseconds)
import Effect.Aff (Aff, delay, forkAff, joinFiber)


class Background (m :: Type -> Type) where
    schedule :: forall a. m a -> Milliseconds -> m Unit


instance affBackground :: Background Aff where
    schedule :: forall a. Aff a -> Milliseconds -> Aff Unit
    schedule action time = do
       delay time
       fiber <- forkAff (void $ action)
       joinFiber fiber

