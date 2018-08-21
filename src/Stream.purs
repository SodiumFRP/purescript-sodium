module SodiumFRP.Stream (
    mapTo,
    orElse,
    merge,
    filter,
    gate,
    snapshot1,
    snapshot,
    snapshot3,
    snapshot4,
    snapshot5,
    snapshot6,
    hold,
    collect,
    accum,
    once,
    loop
) where

import SodiumFRP.Class (
    Stream,
    StreamLoop,
    toStream,
    class SodiumStream,
    Cell,
    toCell,
    class SodiumCell
)

import Prelude 
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Data.Function.Uncurried (
    Fn2, runFn2, mkFn2, 
    Fn3, runFn3, mkFn3, 
    Fn4, runFn4, mkFn4, 
    Fn5, runFn5, mkFn5,
    Fn6, runFn6, mkFn6,
    Fn7, runFn7
)



-- | Transform the stream's event values into the specified constant value.
-- b is a constant value.
mapTo :: forall a b s. (SodiumStream s) => b -> s a -> Stream b
mapTo x s = runFn2 mapToImpl x (toStream s)

{-|
    Variant of 'merge' that merges two streams and will drop an event
    in the simultaneous case of two events in the same Transaction

    The events from the second stream take priority here

    If you want to specify your own merging function, use 'merge'
-}

orElse :: forall a s. (SodiumStream s) => s a -> s a -> Stream a
orElse s1 s2 = runFn2 orElseImpl (toStream s1) (toStream s2)


{-|

    Merges two streams and will drop an event
    in the simultaneous case of two events in the same Transaction

    The supplied function determines which event to drop
    
    It may construct FRP logic or use sample()
    Apart from this the function must be referentially transparent.
-}

merge :: forall a s. (SodiumStream s) => (a -> a -> a) -> s a -> s a -> Stream a
merge f s1 s2 = runFn3 mergeImpl (mkFn2 f) (toStream s1) (toStream s2)

-- | Return a stream that only outputs events for which the predicate returns true.
filter :: forall a s. (SodiumStream s) => (a -> Boolean) -> s a -> Stream a
filter f s = runFn2 filterImpl f (toStream s)

{-|
    Return a stream that only outputs events from the input stream
    when the specified cell's value is true.
-}

gate :: forall a s c. (SodiumStream s) => (SodiumCell c) => c Boolean -> s a -> Stream a
gate c s = runFn2 gateImpl (toCell c) (toStream s)


{-|
    Variant of 'snapshot' that captures the cell's value
    at the time of the event firing, ignoring the stream's value.
-}
snapshot1 :: forall a s c. (SodiumStream s) => (SodiumCell c) => c a -> s a -> Stream a
snapshot1 c s = runFn2 snapshot1Impl (toCell c) (toStream s)

{-|
    Return a stream whose events are the result of the combination using the specified
    function of the input stream's event value and the value of the cell at that time.
    
    There is an implicit delay: State updates caused by event firings being held with
    'hold' don't become visible as the cell's current value until the following transaction. 
    To put this another way, 'snapshot' always sees the value of a cell as it was 
    before any state changes from the current transaction.
-}

snapshot :: forall a b c cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c) -> cel b -> str a -> Stream c
snapshot f c s = runFn3 snapshotImpl (mkFn2 f) (toCell c) (toStream s)
         
snapshot3 :: forall a b c d cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c -> d) -> cel b -> cel c -> str a -> Stream d
snapshot3 f c1 c2 s = runFn4 snapshot3Impl (mkFn3 f) (toCell c1) (toCell c2) (toStream s)

snapshot4 :: forall a b c d e cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c -> d -> e) -> cel b -> cel c -> cel d -> str a -> Stream e
snapshot4 f c1 c2 c3 s = runFn5 snapshot4Impl (mkFn4 f) (toCell c1) (toCell c2) (toCell c3) (toStream s)

snapshot5 :: forall a b c d e f cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c -> d -> e -> f) -> cel b -> cel c -> cel d -> cel e -> str a -> Stream f
snapshot5 f c1 c2 c3 c4 s = runFn6 snapshot5Impl (mkFn5 f) (toCell c1) (toCell c2) (toCell c3) (toCell c4) (toStream s)

snapshot6 :: forall a b c d e f g cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c -> d -> e -> f -> g) -> cel b -> cel c -> cel d -> cel e -> cel f -> str a -> Stream g
snapshot6 f c1 c2 c3 c4 c5 s = runFn7 snapshot6Impl (mkFn6 f) (toCell c1) (toCell c2) (toCell c3) (toCell c4) (toCell c5) (toStream s)


{-|
    Create a "Cell" with the specified initial value, that is updated by this stream's event values.
    There is an implicit delay: State updates caused by event firings don't become
    visible as the cell's current value as viewed by 'snapshot' until the following transaction. 
    To put this another way, 'snapshot' always sees the value of a cell as it was before
    any state changes from the current transaction.
-}

hold :: forall a s. (SodiumStream s) => a -> s a -> Cell a
hold x s = runFn2 holdImpl x (toStream s)

{-|
    Transform an event with a generalized state loop (a Mealy machine). The function
    is passed the input and the old state and returns the new state and output value.
    The function may construct FRP logic or use 'sample' in which case 
    it is equivalent to 'snapshot'ing the cell. 
    Apart from this the function must be referentially transparent.
-}

collect :: forall a b c s. (SodiumStream s) => (a -> c -> {value :: b, state :: c}) -> c -> s a -> Stream b
collect f x s = runFn3 collectImpl (mkFn2 f) x (toStream s)


{-|
    Accumulate on input event, outputting the new state each time.
    The function may construct FRP logic or use 'sample' 
    in which case it is equivalent to 'snapshot'ing the cell. 
    Apart from this the function must be referentially transparent.
-}

accum :: forall a b s. (SodiumStream s) => (a -> b -> b) -> b -> s a -> Cell b
accum f x s = runFn3 accumImpl (mkFn2 f) x (toStream s)

{-|
    Return a stream that outputs only one value: the next event of the
    input stream, starting from the transaction in which once() was invoked.
-}

once :: forall a s. (SodiumStream s) => s a -> Stream a
once s = onceImpl (toStream s)

{-|
    Resolve the loop to specify what the StreamLoop was a forward reference to. 
    It must be invoked inside the same transaction as the place where the StreamLoop is used.
    This requires you to create an explicit transaction 
-}
loop :: forall a s. (SodiumStream s) => s a -> StreamLoop a -> Effect Unit
loop s = runEffectFn2 loopStreamImpl (toStream s)

-- Foreign imports

foreign import mapToImpl :: forall a b. Fn2 b (Stream a) (Stream b)
foreign import orElseImpl :: forall a. Fn2 (Stream a) (Stream a) (Stream a)
foreign import mergeImpl :: forall a. Fn3 (Fn2 a a a) (Stream a) (Stream a) (Stream a)
foreign import filterImpl :: forall a. Fn2 (a -> Boolean) (Stream a) (Stream a)
foreign import gateImpl :: forall a. Fn2 (Cell Boolean) (Stream a) (Stream a)
foreign import snapshot1Impl :: forall a. Fn2 (Cell a) (Stream a) (Stream a)
foreign import snapshotImpl :: forall a b c. Fn3 (Fn2 a b c) (Cell b) (Stream a) (Stream c)
foreign import snapshot3Impl :: forall a b c d. Fn4 (Fn3 a b c d) (Cell b) (Cell c) (Stream a) (Stream d)
foreign import snapshot4Impl :: forall a b c d e. Fn5 (Fn4 a b c d e) (Cell b) (Cell c) (Cell d) (Stream a) (Stream e)
foreign import snapshot5Impl :: forall a b c d e f. Fn6 (Fn5 a b c d e f) (Cell b) (Cell c) (Cell d) (Cell e) (Stream a) (Stream f)
foreign import snapshot6Impl :: forall a b c d e f g. Fn7 (Fn6 a b c d e f g) (Cell b) (Cell c) (Cell d) (Cell e) (Cell f) (Stream a) (Stream g)
foreign import holdImpl :: forall a. Fn2 a (Stream a) (Cell a)
foreign import collectImpl :: forall a b c. Fn3 (Fn2 a c {value :: b, state :: c}) c (Stream a) (Stream b)
foreign import accumImpl :: forall a b. Fn3 (Fn2 a b b) b (Stream a) (Cell b)
foreign import onceImpl :: forall a. Stream a -> Stream a

foreign import loopStreamImpl :: forall a. EffectFn2 (Stream a) (StreamLoop a) Unit
