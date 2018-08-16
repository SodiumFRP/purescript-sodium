module SodiumFRP.Stream (
    mapTo,
    orElse,
    merge,
    filter,
    gate,
    snapshot1
) where

import SodiumFRP.Class (
    Stream,
    Cell
)

import Data.Function.Uncurried (Fn2, runFn2, mkFn2, Fn3, runFn3)

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

snapshot1 :: forall a. Cell a -> Stream a -> Stream a
snapshot1 = runFn2 snapshot1Impl



-- Foreign imports

foreign import mapToImpl :: forall a b. Fn2 b (Stream a) (Stream b)
foreign import orElseImpl :: forall a. Fn2 (Stream a) (Stream a) (Stream a)
foreign import mergeImpl :: forall a. Fn3 (Fn2 a a a) (Stream a) (Stream a) (Stream a)
foreign import filterImpl :: forall a. Fn2 (a -> Boolean) (Stream a) (Stream a)
foreign import gateImpl :: forall a. Fn2 (Cell Boolean) (Stream a) (Stream a)
foreign import snapshot1Impl :: forall a. Fn2 (Cell a) (Stream a) (Stream a)
