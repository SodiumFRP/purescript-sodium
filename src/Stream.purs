module SodiumFRP.Stream (
    Stream,
    StreamSink,
    toStream,
    newStreamSink,
    listen,
    send,
    mapTo
) where

import Prelude
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)
import Data.Function.Uncurried (Fn2, runFn2, mkFn2)

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


--Foreign imports : StreamSink

foreign import newStreamSinkImpl :: forall a. Nullable (Fn2 a a a) -> StreamSink a
foreign import sendImpl :: forall a. EffectFn2 a (StreamSink a) Unit
foreign import toStreamImpl :: forall a. StreamSink a -> Stream a
