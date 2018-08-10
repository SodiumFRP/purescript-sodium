module SodiumFRP.Stream (
    Stream,
    StreamSink,
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
data Stream a = StreamVertex a | StreamSink a

foreign import listenImpl :: forall a. EffectFn2 (Stream a) (EffectFn1 a Unit) (Effect Unit)
foreign import mapStream :: forall a b. (a -> b) -> Stream a -> Stream b

instance functorStream :: Functor Stream where
    map f s = mapStream f s

listen :: forall a. Stream a -> (a -> Effect Unit) -> Effect (Effect Unit)
listen s cb = runEffectFn2 listenImpl s (mkEffectFn1 cb)


-- StreamSink

foreign import data StreamSink :: Type -> Type
foreign import newStreamSinkImpl :: forall a. Nullable (a -> a -> a) -> StreamSink a
foreign import sendImpl :: forall a. EffectFn2 (StreamSink a) a Unit

newStreamSink :: forall a. Maybe (a -> a -> a) -> StreamSink a
newStreamSink a = newStreamSinkImpl (toNullable a)

send :: forall a. StreamSink a -> a -> Effect Unit
send s a = runEffectFn2 sendImpl s a

-- StreamVertex
foreign import data StreamVertex :: Type -> Type
foreign import data Vertex :: Type
foreign import newStreamVertexImpl :: forall a. Nullable (Vertex) -> StreamVertex a

newStreamVertex :: forall a. Maybe Vertex -> StreamVertex a
newStreamVertex v = newStreamVertexImpl (toNullable v)
