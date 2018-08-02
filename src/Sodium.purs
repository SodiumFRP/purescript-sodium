module FRP.Sodium (
    Stream,
    newStreamSink,
    listen,
    send
) where

import Prelude
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)


foreign import data Stream :: Type -> Type

newStreamSink :: forall a. Maybe (a -> a -> a) -> Stream a
newStreamSink a = newStreamSinkImpl (toNullable a)

instance functorStream :: Functor Stream where
    map f s = mapStream f s

listen :: forall a. Stream a -> (a -> Effect Unit) -> Effect (Effect Unit)
listen s cb = runEffectFn2 listenImpl s (mkEffectFn1 cb)

send :: forall a. Stream a -> a -> Effect Unit
send s a = runEffectFn2 sendImpl s a

foreign import listenImpl :: forall a. EffectFn2 (Stream a) (EffectFn1 a Unit) (Effect Unit)
foreign import sendImpl :: forall a. EffectFn2 (Stream a) a Unit
foreign import mapStream :: forall a b. (a -> b) -> Stream a -> Stream b
foreign import newStreamSinkImpl :: forall a. Nullable (a -> a -> a) -> Stream a
