module SodiumFRP.Class where

import Prelude
import Effect (Effect)
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried (Fn2, runFn2, mkFn2)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)

-- | Sodium Classes

foreign import data Stream :: Type -> Type 
foreign import data StreamSink :: Type -> Type 
foreign import data StreamLoop :: Type -> Type
foreign import data Cell :: Type -> Type 
foreign import data CellSink :: Type -> Type 
foreign import data CellLoop :: Type -> Type

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


-- | Create a new Stream
newStream :: forall a. Effect (Stream a)
newStream = newStreamImpl

foreign import newStreamImpl :: forall a. Effect (Stream a)

-- | Create a new StreamSink
-- StreamSinks can be used to send events
-- The optional value is merging function
newStreamSink :: forall a. Maybe (a -> a -> a) -> Effect (StreamSink a)
newStreamSink m = 
    runEffectFn1 newStreamSinkImpl (toNullable (mkFn2 <$> m))

foreign import newStreamSinkImpl :: forall a. EffectFn1 (Nullable (Fn2 a a a)) (StreamSink a)

-- | A forward reference for a 'Stream' equivalent to the Stream that is referenced.
-- Must be run in an explicit Transaction
newStreamLoop :: forall a. Effect (StreamLoop a)
newStreamLoop = newStreamLoopImpl


foreign import newStreamLoopImpl :: forall a. Effect (StreamLoop a)

-- | Convert a Stream
-- This is a free operation, just to help the type system

class SodiumStream s where
    toStream :: forall a. s a -> Stream a

instance streamToStream :: SodiumStream Stream where
    toStream = identity

instance streamSinkToStream :: SodiumStream StreamSink where
    toStream = unsafeCoerce 

instance streamLoopToStream :: SodiumStream StreamLoop where
    toStream = unsafeCoerce 

-- | Functor
instance functorStream :: Functor Stream where
    map = runFn2 mapStreamImpl

foreign import mapStreamImpl :: forall a b. Fn2 (a -> b) (Stream a) (Stream b)

-- | Listen
instance listenStream :: Listenable Stream where
    listen s cb = runEffectFn2 listenStreamImpl s (mkEffectFn1 cb)

foreign import listenStreamImpl :: forall a. EffectFn2 (Stream a) (EffectFn1 a Unit) (Effect Unit)

-- | Send
instance sendStream :: Sendable StreamSink where
    send = runEffectFn2 sendStreamImpl

foreign import sendStreamImpl :: forall a. EffectFn2 a (StreamSink a) Unit

-- | Cell

-- | Create a new Cell
newCell :: forall a. a -> Maybe (Stream a) -> Effect (Cell a)
newCell a s = runEffectFn2 newCellImpl a (toNullable s)

foreign import newCellImpl :: forall a. EffectFn2 a (Nullable (Stream a)) (Cell a)

newCellSink :: forall a. a -> Maybe (a -> a -> a) -> Effect (CellSink a)
newCellSink a m = runEffectFn2 newCellSinkImpl a (toNullable (mkFn2 <$> m))

foreign import newCellSinkImpl :: forall a. EffectFn2 a (Nullable (Fn2 a a a)) (CellSink a)

-- | A forward reference for a 'Cell' equivalent to the Cell that is referenced.
-- Must be run in an explicit Transaction
newCellLoop :: forall a. Effect (CellLoop a)
newCellLoop = newCellLoopImpl

foreign import newCellLoopImpl :: forall a. Effect (CellLoop a)

-- | Convert a Cell

class SodiumCell c where
    toCell :: forall a. c a -> Cell a

instance cellToCell :: SodiumCell Cell where
    toCell = identity

instance cellSinkToCell :: SodiumCell CellSink where
    toCell = unsafeCoerce 

instance cellLoopToCell :: SodiumCell CellLoop where
    toCell = unsafeCoerce 

-- | Functor
instance functorCell :: Functor Cell where
    map = runFn2 mapCellImpl

foreign import mapCellImpl :: forall a b. Fn2 (a -> b) (Cell a) (Cell b)

-- | Listen
instance listenCell :: Listenable Cell where
    listen s cb = runEffectFn2 listenCellImpl s (mkEffectFn1 cb)

foreign import listenCellImpl :: forall a. EffectFn2 (Cell a) (EffectFn1 a Unit) (Effect Unit)
-- | Send
instance sendCell :: Sendable CellSink where
    send = runEffectFn2 sendCellImpl

foreign import sendCellImpl :: forall a. EffectFn2 a (CellSink a) Unit

