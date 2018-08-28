module SodiumFRP.Lambda where

import SodiumFRP.Class(
    Stream,
    StreamSink,
    Cell,
    CellSink,
    class SodiumCell,
    class SodiumStream,
    toCell,
    toStream
)

import Unsafe.Coerce (unsafeCoerce)

import Data.Function.Uncurried (
    Fn2, mkFn2, 
    Fn3, runFn3, mkFn3, 
    Fn4, runFn4, mkFn4, 
    Fn5, runFn5, mkFn5,
    Fn6, runFn6, mkFn6,
    Fn7, runFn7, 
    Fn8, runFn8
)

-- | Lambda wrappers require dependencies

foreign import data Dep :: Type

class IsDep target where
    mkDep :: forall a. target a -> Dep

instance isDepStream :: IsDep Stream where
    mkDep = unsafeCoerce

instance isDepCell :: IsDep Cell where
    mkDep = unsafeCoerce

instance isDepStreamSink :: IsDep StreamSink where
    mkDep = unsafeCoerce

instance isDepCellSink :: IsDep CellSink where
    mkDep = unsafeCoerce

-- Lambda1
class Lambda1 target where
    mapLambda1 :: forall a b. (a -> b) -> Array Dep -> target a -> target b

-- Stream
instance lambda1Stream :: Lambda1 Stream where
    mapLambda1 = runFn3 mapLambda1StreamImpl


snapshotLambda :: forall a b c cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c) -> Array Dep -> cel b -> str a -> Stream c
snapshotLambda f d c s = runFn4 snapshotLambdaImpl (mkFn2 f) d (toCell c) (toStream s)
         
snapshot3Lambda :: forall a b c d cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c -> d) -> Array Dep -> cel b -> cel c -> str a -> Stream d
snapshot3Lambda f d c1 c2 s = runFn5 snapshot3LambdaImpl (mkFn3 f) d (toCell c1) (toCell c2) (toStream s)

snapshot4Lambda :: forall a b c d e cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c -> d -> e) -> Array Dep -> cel b -> cel c -> cel d -> str a -> Stream e
snapshot4Lambda f d c1 c2 c3 s = runFn6 snapshot4LambdaImpl (mkFn4 f) d (toCell c1) (toCell c2) (toCell c3) (toStream s)

snapshot5Lambda :: forall a b c d e f cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c -> d -> e -> f) -> Array Dep -> cel b -> cel c -> cel d -> cel e -> str a -> Stream f
snapshot5Lambda f d c1 c2 c3 c4 s = runFn7 snapshot5LambdaImpl (mkFn5 f) d (toCell c1) (toCell c2) (toCell c3) (toCell c4) (toStream s)

snapshot6Lambda :: forall a b c d e f g cel str. (SodiumStream str) => (SodiumCell cel) => (a -> b -> c -> d -> e -> f -> g) -> Array Dep -> cel b -> cel c -> cel d -> cel e -> cel f -> str a -> Stream g
snapshot6Lambda f d c1 c2 c3 c4 c5 s = runFn8 snapshot6LambdaImpl (mkFn6 f) d (toCell c1) (toCell c2) (toCell c3) (toCell c4) (toCell c5) (toStream s)



foreign import mapLambda1StreamImpl :: forall a b c. Fn3 (a -> b) (Array c) (Stream a) (Stream b)

foreign import snapshotLambdaImpl :: forall a b c. Fn4 (Fn2 a b c) (Array Dep) (Cell b) (Stream a) (Stream c)
foreign import snapshot3LambdaImpl :: forall a b c d. Fn5 (Fn3 a b c d) (Array Dep) (Cell b) (Cell c) (Stream a) (Stream d)
foreign import snapshot4LambdaImpl :: forall a b c d e. Fn6 (Fn4 a b c d e) (Array Dep) (Cell b) (Cell c) (Cell d) (Stream a) (Stream e)
foreign import snapshot5LambdaImpl :: forall a b c d e f. Fn7 (Fn5 a b c d e f) (Array Dep) (Cell b) (Cell c) (Cell d) (Cell e) (Stream a) (Stream f)
foreign import snapshot6LambdaImpl :: forall a b c d e f g. Fn8 (Fn6 a b c d e f g) (Array Dep) (Cell b) (Cell c) (Cell d) (Cell e) (Cell f) (Stream a) (Stream g)

-- Cell 
instance lambda1Cell :: Lambda1 Cell where
    mapLambda1 = runFn3 mapLambda1CellImpl

foreign import mapLambda1CellImpl :: forall a b c. Fn3 (a -> b) (Array c) (Cell a) (Cell b)


liftLambda :: forall a b c cel. (SodiumCell cel) => (a -> b -> c) -> Array Dep -> cel b -> cel a -> Cell c
liftLambda f d c1 c2 = runFn4 liftLambdaImpl (mkFn2 f) d (toCell c1) (toCell c2)
         
lift3Lambda :: forall a b c d cel. (SodiumCell cel) => (a -> b -> c -> d) -> Array Dep -> cel b -> cel c -> cel a -> Cell d
lift3Lambda f d c1 c2 c3 = runFn5 lift3LambdaImpl (mkFn3 f) d (toCell c1) (toCell c2) (toCell c3)

lift4Lambda :: forall a b c d e cel. (SodiumCell cel) => (a -> b -> c -> d -> e) -> Array Dep -> cel b -> cel c -> cel d -> cel a -> Cell e
lift4Lambda f d c1 c2 c3 c4 = runFn6 lift4LambdaImpl (mkFn4 f) d (toCell c1) (toCell c2) (toCell c3) (toCell c4)

lift5Lambda :: forall a b c d e f cel. (SodiumCell cel) => (a -> b -> c -> d -> e -> f) -> Array Dep -> cel b -> cel c -> cel d -> cel e -> cel a -> Cell f
lift5Lambda f d c1 c2 c3 c4 c5 = runFn7 lift5LambdaImpl (mkFn5 f) d (toCell c1) (toCell c2) (toCell c3) (toCell c4) (toCell c5)

lift6Lambda :: forall a b c d e f g cel. (SodiumCell cel) => (a -> b -> c -> d -> e -> f -> g) -> Array Dep -> cel b -> cel c -> cel d -> cel e -> cel f -> cel a -> Cell g
lift6Lambda f d c1 c2 c3 c4 c5 c6 = runFn8 lift6LambdaImpl (mkFn6 f) d (toCell c1) (toCell c2) (toCell c3) (toCell c4) (toCell c5) (toCell c6)

foreign import liftLambdaImpl :: forall a b c. Fn4 (Fn2 a b c) (Array Dep) (Cell b) (Cell a) (Cell c)
foreign import lift3LambdaImpl :: forall a b c d. Fn5 (Fn3 a b c d) (Array Dep) (Cell b) (Cell c) (Cell a) (Cell d)
foreign import lift4LambdaImpl :: forall a b c d e. Fn6 (Fn4 a b c d e) (Array Dep) (Cell b) (Cell c) (Cell d) (Cell a) (Cell e)
foreign import lift5LambdaImpl :: forall a b c d e f. Fn7 (Fn5 a b c d e f) (Array Dep) (Cell b) (Cell c) (Cell d) (Cell e) (Cell a) (Cell f)
foreign import lift6LambdaImpl :: forall a b c d e f g. Fn8 (Fn6 a b c d e f g) (Array Dep) (Cell b) (Cell c) (Cell d) (Cell e) (Cell f) (Cell a) (Cell g)
