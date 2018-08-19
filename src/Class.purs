module SodiumFRP.Class where

import Prelude
import Effect (Effect)
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried (Fn2, runFn2, mkFn2)
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)

-- | Sodium Classes

foreign import data Stream :: Type -> Type 
foreign import data StreamSink :: Type -> Type 
foreign import data Cell :: Type -> Type 
foreign import data CellSink :: Type -> Type 

-- Common Typeclasses

-- | Listenable
-- Listen for firings of this cell or event.
-- The returned Effect is a function that unregisteres the listener
-- This is the observer pattern.

class Listenable l where
    listen :: forall a. l a -> (a -> Effect Unit) -> Effect (Effect Unit)

-- | Sendable
-- Send events or change of behavior
class Sendable s where
    send :: forall a. a -> s a -> Effect Unit

-- Stream 

class ToStream s where
    toStream :: forall a. s a -> Stream a

-- | Create a new Stream
newStream :: forall a. Stream a
newStream = newStreamImpl

-- | Create a new StreamSink
-- StreamSinks can be used to send events
-- The optional value is merging function
newStreamSink :: forall a. Maybe (a -> a -> a) -> StreamSink a
newStreamSink m = 
    newStreamSinkImpl (toNullable (mkFn2 <$> m))

-- | Convert a StreamSink to a Stream
-- This is a free operation, just to help the type system

instance streamSinkToStream :: ToStream StreamSink where
    toStream = toStreamImpl

instance functorStream :: Functor Stream where
    map = runFn2 mapStreamImpl

instance listenStream :: Listenable Stream where
    listen s cb = runEffectFn2 listenStreamImpl s (mkEffectFn1 cb)

instance sendStream :: Sendable StreamSink where
    send = runEffectFn2 sendStreamImpl



-- | Cell

class ToCell c where
    toCell :: forall a. c a -> Cell a

-- | Create a new Cell
newCell :: forall a. a -> Maybe (Stream a) -> Cell a
newCell a s = runFn2 newCellImpl a (toNullable s)

newCellSink :: forall a. a -> Maybe (a -> a -> a) -> CellSink a
newCellSink a m = runFn2 newCellSinkImpl a (toNullable (mkFn2 <$> m))

instance cellSinkToCell :: ToCell CellSink where
    toCell = toCellImpl

instance functorCell :: Functor Cell where
    map = runFn2 mapCellImpl

instance listenCell :: Listenable Cell where
    listen s cb = runEffectFn2 listenCellImpl s (mkEffectFn1 cb)

instance sendCell :: Sendable CellSink where
    send = runEffectFn2 sendCellImpl



-- Stream FFI
foreign import newStreamImpl :: forall a. Stream a

foreign import newStreamSinkImpl :: forall a. Nullable (Fn2 a a a) -> StreamSink a
foreign import toStreamImpl :: forall a. StreamSink a -> Stream a

foreign import listenStreamImpl :: forall a. EffectFn2 (Stream a) (EffectFn1 a Unit) (Effect Unit)

foreign import mapStreamImpl :: forall a b. Fn2 (a -> b) (Stream a) (Stream b)

foreign import sendStreamImpl :: forall a. EffectFn2 a (StreamSink a) Unit

--  Cell FFI
foreign import newCellImpl :: forall a. Fn2 a (Nullable (Stream a)) (Cell a)

foreign import newCellSinkImpl :: forall a. Fn2 a (Nullable (Fn2 a a a)) (CellSink a)

foreign import toCellImpl :: forall a. CellSink a -> Cell a

foreign import listenCellImpl :: forall a. EffectFn2 (Cell a) (EffectFn1 a Unit) (Effect Unit)

foreign import mapCellImpl :: forall a b. Fn2 (a -> b) (Cell a) (Cell b)


foreign import sendCellImpl :: forall a. EffectFn2 a (CellSink a) Unit

