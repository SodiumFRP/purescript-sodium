module SodiumFRP.Cell (
    sample,
    loopCell,
    lift,
    lift3,
    lift4,
    lift5,
    lift6,
    switchC,
    switchS
) where

import SodiumFRP.Class(
    Cell,
    CellLoop,
    toCell,
    class SodiumCell,
    Stream
)

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2)
import Data.Function.Uncurried (
    Fn1, runFn1,
    Fn2, mkFn2, 
    Fn3, runFn3, mkFn3, 
    Fn4, runFn4, mkFn4, 
    Fn5, runFn5, mkFn5,
    Fn6, runFn6, mkFn6,
    Fn7, runFn7
)

-- | Sample 
sample :: forall a c. (SodiumCell c) => c a -> a
sample c = runFn1 sampleImpl (toCell c)

foreign import sampleImpl :: forall a. Fn1 (Cell a) (a)

-- Loop
-- | Resolve the loop to specify what the CellLoop was a forward reference to. 
-- | It must be invoked inside the same transaction as the place where the CellLoop is used.
-- | This requires you to create an explicit transaction 
loopCell :: forall a c. (SodiumCell c) => c a -> CellLoop a -> Effect Unit
loopCell c = runEffectFn2 loopCellImpl (toCell c)

foreign import loopCellImpl :: forall a. EffectFn2 (Cell a) (CellLoop a) Unit

-- Lift
lift :: forall a b c cel. (SodiumCell cel) => (a -> b -> c) -> cel b -> cel a -> Cell c
lift f c1 c2 = runFn3 liftImpl (mkFn2 f) (toCell c1) (toCell c2)
         
lift3 :: forall a b c d cel. (SodiumCell cel) => (a -> b -> c -> d) -> cel b -> cel c -> cel a -> Cell d
lift3 f c1 c2 c3 = runFn4 lift3Impl (mkFn3 f) (toCell c1) (toCell c2) (toCell c3)

lift4 :: forall a b c d e cel. (SodiumCell cel) => (a -> b -> c -> d -> e) -> cel b -> cel c -> cel d -> cel a -> Cell e
lift4 f c1 c2 c3 c4 = runFn5 lift4Impl (mkFn4 f) (toCell c1) (toCell c2) (toCell c3) (toCell c4)

lift5 :: forall a b c d e f cel. (SodiumCell cel) => (a -> b -> c -> d -> e -> f) -> cel b -> cel c -> cel d -> cel e -> cel a -> Cell f
lift5 f c1 c2 c3 c4 c5 = runFn6 lift5Impl (mkFn5 f) (toCell c1) (toCell c2) (toCell c3) (toCell c4) (toCell c5)

lift6 :: forall a b c d e f g cel. (SodiumCell cel) => (a -> b -> c -> d -> e -> f -> g) -> cel b -> cel c -> cel d -> cel e -> cel f -> cel a -> Cell g
lift6 f c1 c2 c3 c4 c5 c6 = runFn7 lift6Impl (mkFn6 f) (toCell c1) (toCell c2) (toCell c3) (toCell c4) (toCell c5) (toCell c6)

foreign import liftImpl :: forall a b c. Fn3 (Fn2 a b c) (Cell b) (Cell a) (Cell c)
foreign import lift3Impl :: forall a b c d. Fn4 (Fn3 a b c d) (Cell b) (Cell c) (Cell a) (Cell d)
foreign import lift4Impl :: forall a b c d e. Fn5 (Fn4 a b c d e) (Cell b) (Cell c) (Cell d) (Cell a) (Cell e)
foreign import lift5Impl :: forall a b c d e f. Fn6 (Fn5 a b c d e f) (Cell b) (Cell c) (Cell d) (Cell e) (Cell a) (Cell f)
foreign import lift6Impl :: forall a b c d e f g. Fn7 (Fn6 a b c d e f g) (Cell b) (Cell c) (Cell d) (Cell e) (Cell f) (Cell a) (Cell g)

-- Switch
switchC :: forall a c. (SodiumCell c) => c (Cell a) -> Effect (Cell a)
switchC c = runEffectFn1 switchCImpl (toCell c) 

switchS :: forall a c. (SodiumCell c) => c (Stream a) -> Stream a
switchS c = runFn1 switchSImpl (toCell c) 

foreign import switchCImpl :: forall a. EffectFn1 (Cell (Cell a)) (Cell a)
foreign import switchSImpl :: forall a. Fn1 (Cell (Stream a)) (Stream a)

