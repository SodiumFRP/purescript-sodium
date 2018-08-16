module SodiumFRP.Stream (
    send,
    mapTo,
    orElse,
    merge,
    filter
) where

import SodiumFRP.Class (
    Stream,
    StreamSink,
    Cell
)

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Data.Function.Uncurried (Fn2, runFn2, mkFn2, Fn3, runFn3)

-- Stream 

-- | Transform the stream's event values into the specified constant value.
-- b is a constant value.
mapTo :: forall a b. b -> Stream a -> Stream b
mapTo = runFn2 mapToImpl

{-|
    Variant of merge() that merges two streams and will drop an event
    in the simultaneous case of two events in the same Transaction

    The events from the second stream take priority here

    If you want to specify your own merging function, use merge()
-}

orElse :: forall a. Stream a -> Stream a -> Stream a
orElse = runFn2 orElseImpl


{-|

    Merges two streams and will drop an event
    in the simultaneous case of two events in the same Transaction

    The supplied function determines which event to drop
    
    It may construct FRP logic or use sample()
    Apart from this the function must be referentially transparent.
-}

merge :: forall a. (a -> a -> a) -> Stream a -> Stream a -> Stream a
merge f = runFn3 mergeImpl (mkFn2 f)

-- | Return a stream that only outputs events for which the predicate returns true.
filter :: forall a. (a -> Boolean) -> Stream a -> Stream a
filter = runFn2 filterImpl

{-|
    Return a stream that only outputs events from the input stream
    when the specified cell's value is true.
-}

gate :: forall a. Cell Boolean -> Stream a -> Stream a
gate = runFn2 gateImpl

-- StreamSink


-- | Send an Event to the given StreamSink 
send :: forall a. a -> StreamSink a -> Effect Unit
send = runEffectFn2 sendImpl

--Foreign imports : Stream


foreign import mapToImpl :: forall a b. Fn2 b (Stream a) (Stream b)
foreign import orElseImpl :: forall a. Fn2 (Stream a) (Stream a) (Stream a)
foreign import mergeImpl :: forall a. Fn3 (Fn2 a a a) (Stream a) (Stream a) (Stream a)
foreign import filterImpl :: forall a. Fn2 (a -> Boolean) (Stream a) (Stream a)
foreign import gateImpl :: forall a. Fn2 (Cell Boolean) (Stream a) (Stream a)

--Foreign imports : StreamSink

foreign import sendImpl :: forall a. EffectFn2 a (StreamSink a) Unit
