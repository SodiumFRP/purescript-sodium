module SodiumFRP.Stream (
    Stream,
    StreamSink,
    toStream,
    newStreamSink,
    listen,
    send,
    mapTo,
    orElse,
    merge
) where

import Prelude
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)
import Data.Function.Uncurried (Fn2, runFn2, mkFn2, Fn3, runFn3)

-- Stream 
data Stream a = Stream a

instance functorStream :: Functor Stream where
    map = runFn2 mapImpl

-- | Create a new Stream
-- The optional value is a Vertex (internal use only?)
newStream :: forall a. Maybe Vertex -> Stream a
newStream v = newStreamImpl (toNullable v)

-- | Listen for firings of this event.
-- The returned Effect is an is a function that unregisteres the listener
-- This is the observer pattern.
listen :: forall a. Stream a -> (a -> Effect Unit) -> Effect (Effect Unit)
listen s cb = runEffectFn2 listenImpl s (mkEffectFn1 cb)

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

-- StreamSink
data StreamSink a = StreamSink a

-- | Create a new StreamSink
-- StreamSinks can be used to send events
-- The optional value is merging function
newStreamSink :: forall a. Maybe (a -> a -> a) -> StreamSink a
newStreamSink m = 
    newStreamSinkImpl (toNullable (mkFn2 <$> m))

-- | Convert a StreamSink to a Stream
-- This is a free operation, just to help the type system
toStream :: forall a. StreamSink a -> Stream a
toStream = toStreamImpl

-- | Send an Event to the given StreamSink 
send :: forall a. a -> StreamSink a -> Effect Unit
send = runEffectFn2 sendImpl

--Foreign imports : Stream

foreign import data Vertex :: Type

foreign import newStreamImpl :: forall a. Nullable (Vertex) -> Stream a
foreign import listenImpl :: forall a. EffectFn2 (Stream a) (EffectFn1 a Unit) (Effect Unit)
foreign import mapImpl :: forall a b. Fn2 (a -> b) (Stream a) (Stream b)
foreign import mapToImpl :: forall a b. Fn2 b (Stream a) (Stream b)
foreign import orElseImpl :: forall a. Fn2 (Stream a) (Stream a) (Stream a)
foreign import mergeImpl :: forall a. Fn3 (Fn2 a a a) (Stream a) (Stream a) (Stream a)


--Foreign imports : StreamSink

foreign import newStreamSinkImpl :: forall a. Nullable (Fn2 a a a) -> StreamSink a
foreign import sendImpl :: forall a. EffectFn2 a (StreamSink a) Unit
foreign import toStreamImpl :: forall a. StreamSink a -> Stream a
