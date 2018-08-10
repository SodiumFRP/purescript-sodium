module SodiumFRP.Stream (
    Stream,
    StreamSink,
    toStream,
    newStreamSink,
    listen,
    send
) where

import Prelude
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)

-- Stream 
data Stream a = Stream a
foreign import data Vertex :: Type

foreign import newStreamImpl :: forall a. Nullable (Vertex) -> Stream a
foreign import listenImpl :: forall a. EffectFn2 (Stream a) (EffectFn1 a Unit) (Effect Unit)
foreign import mapStream :: forall a b. (a -> b) -> Stream a -> Stream b

newStream :: forall a. Maybe Vertex -> Stream a
newStream v = newStreamImpl (toNullable v)

listen :: forall a. Stream a -> (a -> Effect Unit) -> Effect (Effect Unit)
listen s cb = runEffectFn2 listenImpl s (mkEffectFn1 cb)

instance functorStream :: Functor Stream where
    map f s = mapStream f s



-- StreamSink
data StreamSink a = StreamSink a
foreign import newStreamSinkImpl :: forall a. Nullable (a -> a -> a) -> StreamSink a
foreign import sendImpl :: forall a. EffectFn2 (StreamSink a) a Unit
foreign import toStreamImpl :: forall a. StreamSink a -> Stream a

newStreamSink :: forall a. Maybe (a -> a -> a) -> StreamSink a
newStreamSink a = newStreamSinkImpl (toNullable a)

toStream :: forall a. StreamSink a -> Stream a
toStream s = toStreamImpl s 

send :: forall a. StreamSink a -> a -> Effect Unit
send s a = runEffectFn2 sendImpl s a

-- StreamVertex
