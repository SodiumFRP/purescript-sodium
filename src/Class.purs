module SodiumFRP.Class (
    Stream,
    StreamSink,
    StreamLoop,
    class SodiumStream,
    newStream,
    newStreamSink,
    newStreamLoop,
    toStream,
    Cell,
    CellSink,
    CellLoop,
    class SodiumCell,
    newCell,
    newCellSink,
    newCellLoop,
    toCell,
    class Listenable,
    listen,
    class Sendable,
    send
)
where

import Effect (Effect)

import Prelude 
import Effect.Uncurried (runEffectFn1, mkEffectFn1, EffectFn1, EffectFn2, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried ( Fn0, runFn0, Fn1, runFn1, Fn2, runFn2, mkFn2)

-- Common Typeclasses

-- Listenable
-- | Listen for firings of this cell or event.
-- | The returned Effect is a function that unregisteres the listener
-- | This is the observer pattern.

class Listenable l where
    listen :: forall a. l a -> (a -> Effect Unit) -> Effect (Effect Unit)

-- Sendable
-- | Send events or change of behavior
class Sendable s where
    send :: forall a. a -> s a -> Effect Unit

-- | Constructors

foreign import data Stream :: Type -> Type 
foreign import data Cell :: Type -> Type 
foreign import data StreamSink :: Type -> Type 
foreign import data CellSink :: Type -> Type 
foreign import data StreamLoop :: Type -> Type
foreign import data CellLoop :: Type -> Type

newStream :: forall a. Stream a
newStream = runFn0 newStreamImpl

newCell :: forall a. a -> Cell a
newCell = runFn1 newCellImpl

-- Create a new StreamSink
-- | StreamSinks can be used to send events
-- | The optional value is merging function
newStreamSink :: forall a. Maybe (a -> a -> a) -> Effect (StreamSink a)
newStreamSink m = 
    runEffectFn1 newStreamSinkImpl (toNullable (mkFn2 <$> m))

newCellSink :: forall a. a -> Maybe (a -> a -> a) -> Effect (CellSink a)
newCellSink a m = runEffectFn2 newCellSinkImpl a (toNullable (mkFn2 <$> m))

-- | A forward reference for a 'Stream' equivalent to the Stream that is referenced.
-- | Must be run in an explicit Transaction
newStreamLoop :: forall a. Effect (StreamLoop a)
newStreamLoop = newStreamLoopImpl

-- | A forward reference for a 'Cell' equivalent to the Cell that is referenced.
-- | Must be run in an explicit Transaction
newCellLoop :: forall a. Effect (CellLoop a)
newCellLoop = newCellLoopImpl

foreign import newStreamImpl :: forall a. Fn0 (Stream a)
foreign import newCellImpl :: forall a. Fn1 a (Cell a)

foreign import newStreamSinkImpl :: forall a. EffectFn1 (Nullable (Fn2 a a a)) (StreamSink a)
foreign import newCellSinkImpl :: forall a. EffectFn2 a (Nullable (Fn2 a a a)) (CellSink a)

foreign import newStreamLoopImpl :: forall a. Effect (StreamLoop a)
foreign import newCellLoopImpl :: forall a. Effect (CellLoop a)

-- Convertors

class SodiumStream s where
    toStream :: forall a. s a -> Stream a

class SodiumCell c where
    toCell :: forall a. c a -> Cell a


instance streamToStream :: SodiumStream Stream where
    toStream = unsafeCoerce 
instance cellToCell :: SodiumCell Cell where
    toCell = unsafeCoerce 

instance streamSinkToStream :: SodiumStream StreamSink where
    toStream = unsafeCoerce 
instance cellSinkToCell :: SodiumCell CellSink where
    toCell = unsafeCoerce 

instance streamLoopToStream :: SodiumStream StreamLoop where
    toStream = unsafeCoerce 
instance cellLoopToCell :: SodiumCell CellLoop where
    toCell = unsafeCoerce 

-- Listen

instance listenStream :: Listenable Stream where
    listen s cb = runEffectFn2 listenStreamImpl s (mkEffectFn1 cb)
instance listenCell :: Listenable Cell where
    listen c cb = runEffectFn2 listenCellImpl c (mkEffectFn1 cb)

instance listenStreamSink :: Listenable StreamSink where
    listen s cb = runEffectFn2 listenStreamImpl (toStream s) (mkEffectFn1 cb)

instance listenCellSink :: Listenable CellSink where
    listen c cb = runEffectFn2 listenCellImpl (toCell c) (mkEffectFn1 cb)

instance listenStreamLoop :: Listenable StreamLoop where
    listen s cb = runEffectFn2 listenStreamImpl (toStream s) (mkEffectFn1 cb)
instance listenCellLoop :: Listenable CellLoop where
    listen c cb = runEffectFn2 listenCellImpl (toCell c) (mkEffectFn1 cb)

foreign import listenStreamImpl :: forall a. EffectFn2 (Stream a) (EffectFn1 a Unit) (Effect Unit)
foreign import listenCellImpl :: forall a. EffectFn2 (Cell a) (EffectFn1 a Unit) (Effect Unit)

-- Send

instance sendStream :: Sendable StreamSink where
    send = runEffectFn2 sendStreamImpl
instance sendCell :: Sendable CellSink where
    send = runEffectFn2 sendCellImpl

foreign import sendStreamImpl :: forall a. EffectFn2 a (StreamSink a) Unit
foreign import sendCellImpl :: forall a. EffectFn2 a (CellSink a) Unit

-- Categories (Stream)

instance functorStream :: Functor Stream where
    map = runFn2 mapStreamImpl

instance semigroupStream :: Semigroup (Stream a) where
    append = runFn2 concatStreamImpl

instance monoidStream :: Monoid (Stream a) where
    mempty = newStream 

foreign import mapStreamImpl :: forall a b. Fn2 (a -> b) (Stream a) (Stream b)

foreign import concatStreamImpl :: forall a. Fn2 (Stream a) (Stream a) (Stream a)


-- Categories (Cell)

instance functorCell :: Functor Cell where
    map = runFn2 mapCellImpl

instance applyCell :: Apply Cell where
    apply = runFn2 applyImpl

instance applicativeCell :: Applicative Cell where
    pure = runFn1 newCellImpl

foreign import mapCellImpl :: forall a b. Fn2 (a -> b) (Cell a) (Cell b)

foreign import applyImpl :: forall a b. Fn2 (Cell (a -> b)) (Cell a) (Cell b)
