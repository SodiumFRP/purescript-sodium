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
    once
) where

import SodiumFRP.Class (
    Stream,
    Cell
)

import SodiumFRP.Lambda

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
mapTo :: forall a b. b -> Stream a -> Stream b
mapTo = runFn2 mapToImpl

{-|
    Variant of 'merge' that merges two streams and will drop an event
    in the simultaneous case of two events in the same Transaction

    The events from the second stream take priority here

    If you want to specify your own merging function, use 'merge'
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


{-|
    Variant of 'snapshot' that captures the cell's value
    at the time of the event firing, ignoring the stream's value.
-}
snapshot1 :: forall a. Cell a -> Stream a -> Stream a
snapshot1 = runFn2 snapshot1Impl

{-|
    Return a stream whose events are the result of the combination using the specified
    function of the input stream's event value and the value of the cell at that time.
    
    There is an implicit delay: State updates caused by event firings being held with
    'hold' don't become visible as the cell's current value until the following transaction. 
    To put this another way, 'snapshot' always sees the value of a cell as it was 
    before any state changes from the current transaction.
-}

snapshot :: forall a b c lam. Lambda2 lam a b c => lam -> Cell b -> Stream a -> Stream c
snapshot f = runFn3 snapshotImpl (sodiumLambda2 f)
         
snapshot3 :: forall a b c d. (a -> b -> c -> d) -> Cell b -> Cell c -> Stream a -> Stream d
snapshot3 f = runFn4 snapshot3Impl (mkFn3 f)

snapshot4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Cell b -> Cell c -> Cell d -> Stream a -> Stream e
snapshot4 f = runFn5 snapshot4Impl (mkFn4 f)

snapshot5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Cell b -> Cell c -> Cell d -> Cell e -> Stream a -> Stream f
snapshot5 f = runFn6 snapshot5Impl (mkFn5 f)


snapshot6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Cell b -> Cell c -> Cell d -> Cell e -> Cell f -> Stream a -> Stream g
snapshot6 f = runFn7 snapshot6Impl (mkFn6 f)

{-|
    Create a "Cell" with the specified initial value, that is updated by this stream's event values.
    There is an implicit delay: State updates caused by event firings don't become
    visible as the cell's current value as viewed by 'snapshot' until the following transaction. 
    To put this another way, 'snapshot' always sees the value of a cell as it was before
    any state changes from the current transaction.
-}

hold :: forall a. a -> Stream a -> Cell a
hold = runFn2 holdImpl

{-|
    Transform an event with a generalized state loop (a Mealy machine). The function
    is passed the input and the old state and returns the new state and output value.
    The function may construct FRP logic or use 'sample' in which case 
    it is equivalent to 'snapshot'ing the cell. 
    Apart from this the function must be referentially transparent.
-}

collect :: forall a b c. (a -> c -> {value :: b, state :: c}) -> c -> Stream a -> Stream b
collect f = runFn3 collectImpl (mkFn2 f) 


{-|
    Accumulate on input event, outputting the new state each time.
    The function may construct FRP logic or use 'sample' 
    in which case it is equivalent to 'snapshot'ing the cell. 
    Apart from this the function must be referentially transparent.
-}

accum :: forall a b. (a -> b -> b) -> b -> Stream a -> Cell b
accum f = runFn3 accumImpl (mkFn2 f)

{-|
    Return a stream that outputs only one value: the next event of the
    input stream, starting from the transaction in which once() was invoked.
-}

once :: forall a. Stream a -> Stream a
once = onceImpl

-- Foreign imports

foreign import mapToImpl :: forall a b. Fn2 b (Stream a) (Stream b)
foreign import orElseImpl :: forall a. Fn2 (Stream a) (Stream a) (Stream a)
foreign import mergeImpl :: forall a. Fn3 (Fn2 a a a) (Stream a) (Stream a) (Stream a)
foreign import filterImpl :: forall a. Fn2 (a -> Boolean) (Stream a) (Stream a)
foreign import gateImpl :: forall a. Fn2 (Cell Boolean) (Stream a) (Stream a)
foreign import snapshot1Impl :: forall a. Fn2 (Cell a) (Stream a) (Stream a)
foreign import snapshotImpl :: forall a b c. Fn3 (SodiumLambda2 a b c) (Cell b) (Stream a) (Stream c)
foreign import snapshot3Impl :: forall a b c d. Fn4 (Fn3 a b c d) (Cell b) (Cell c) (Stream a) (Stream d)
foreign import snapshot4Impl :: forall a b c d e. Fn5 (Fn4 a b c d e) (Cell b) (Cell c) (Cell d) (Stream a) (Stream e)
foreign import snapshot5Impl :: forall a b c d e f. Fn6 (Fn5 a b c d e f) (Cell b) (Cell c) (Cell d) (Cell e) (Stream a) (Stream f)
foreign import snapshot6Impl :: forall a b c d e f g. Fn7 (Fn6 a b c d e f g) (Cell b) (Cell c) (Cell d) (Cell e) (Cell f) (Stream a) (Stream g)
foreign import holdImpl :: forall a. Fn2 a (Stream a) (Cell a)
foreign import collectImpl :: forall a b c. Fn3 (Fn2 a c {value :: b, state :: c}) c (Stream a) (Stream b)
foreign import accumImpl :: forall a b. Fn3 (Fn2 a b b) b (Stream a) (Cell b)
foreign import onceImpl :: forall a. Stream a -> Stream a
