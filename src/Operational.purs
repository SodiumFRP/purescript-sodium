module SodiumFRP.Operational (
    updates,
    value,
    defer,
    split
) where 

import SodiumFRP.Class (
    class SodiumCell,
    class SodiumStream,
    Stream,
    Cell,
    toCell,
    toStream
)

import Data.Function.Uncurried (
    Fn1, runFn1    
)

-- | A stream that gives the updates/steps for a 'Cell' 
-- |
-- | This is an OPERATIONAL primitive, which is not part of the main Sodium API. 
-- | It breaks the property of non-detectability of cell steps/updates.
-- | The rule with this primitive is that you should only use it in functions
-- | that do not allow the caller to detect the cell updates.
updates :: forall a c. (SodiumCell c) => c a -> Stream a
updates c = runFn1 updatesImpl (toCell c)

-- | A stream that is guaranteed to fire once in the transaction where value() is invoked, giving
-- | the current value of the cell, and thereafter behaves like 'updates' 
-- | firing for each update/step of the cell's value.
-- |
-- | This is an OPERATIONAL primitive, which is not part of the main Sodium API. 
-- | It breaks the property of non-detectability of cell steps/updates.
-- | The rule with this primitive is that you should only use it in functions
-- | that do not allow the caller to detect the cell updates.
value :: forall a c. (SodiumCell c) => c a -> Stream a
value c = runFn1 valueImpl (toCell c)

-- | Push each event onto a new transaction guaranteed to come before the next externally
-- | initiated transaction. Same as 'split' but it works on a single value.
defer :: forall a s. (SodiumStream s) => s a -> Stream a
defer s = runFn1 deferImpl (toStream s)

-- | Push each event in the list onto a newly created transaction guaranteed
-- | to come before the next externally initiated transaction. Note that the semantics
-- | are such that two different invocations of split() can put events into the same
-- | new transaction, so the resulting stream's events could be simultaneous with
-- | events output by split() or 'defer' invoked elsewhere in the code.
split :: forall a s. (SodiumStream s) => s (Array a) -> Stream a
split sa = runFn1 splitImpl (toStream sa)

foreign import updatesImpl :: forall a. Fn1 (Cell a) (Stream a)
foreign import valueImpl :: forall a. Fn1 (Cell a) (Stream a)
foreign import deferImpl :: forall a. Fn1 (Stream a) (Stream a)
foreign import splitImpl :: forall a. Fn1 (Stream (Array a)) (Stream a)
