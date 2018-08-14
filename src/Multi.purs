module SodiumFRP.Multi where

import Prelude
import Effect (Effect)

-- | Listen for firings of this cell or event.
-- The returned Effect is a function that unregisteres the listener
-- This is the observer pattern.

class Listenable l where
    listen :: forall a. l a -> (a -> Effect Unit) -> Effect (Effect Unit)
