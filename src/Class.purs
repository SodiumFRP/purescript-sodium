module SodiumFRP.Class where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)

-- Stream 
data Stream a = Stream a
data StreamSink a = StreamSink a

instance functorStream :: Functor Stream where
    map = runFn2 mapStreamImpl

instance listenStream :: Listenable Stream where
    listen s cb = runEffectFn2 listenStreamImpl s (mkEffectFn1 cb)


foreign import listenStreamImpl :: forall a. EffectFn2 (Stream a) (EffectFn1 a Unit) (Effect Unit)

foreign import mapStreamImpl :: forall a b. Fn2 (a -> b) (Stream a) (Stream b)

-- Cell
data Cell a = Cell a
data CellSink a = CellSink a

instance functorCell :: Functor Cell where
    map = runFn2 mapCellImpl

instance listenCell :: Listenable Cell where
    listen s cb = runEffectFn2 listenCellImpl s (mkEffectFn1 cb)

foreign import listenCellImpl :: forall a. EffectFn2 (Cell a) (EffectFn1 a Unit) (Effect Unit)
foreign import mapCellImpl :: forall a b. Fn2 (a -> b) (Cell a) (Cell b)


-- | Listenable
-- Listen for firings of this cell or event.
-- The returned Effect is a function that unregisteres the listener
-- This is the observer pattern.

class Listenable l where
    listen :: forall a. l a -> (a -> Effect Unit) -> Effect (Effect Unit)
