module SodiumFRP.Lambda where

import SodiumFRP.Class(
    Stream,
    StreamSink,
    Cell,
    CellSink
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

-- | Lambda1
class Lambda1 target where
    mapLambda1 :: forall a b. (a -> b) -> Array Dep -> target a -> target b

-- | Stream
instance lambda1Stream :: Lambda1 Stream where
    mapLambda1 = runFn3 mapLambda1StreamImpl


snapshotLambda :: forall a b c. (a -> b -> c) -> Array Dep -> Cell b -> Stream a -> Stream c
snapshotLambda f = runFn4 snapshotLambdaImpl (mkFn2 f)
         
snapshot3Lambda :: forall a b c d. (a -> b -> c -> d) -> Array Dep -> Cell b -> Cell c -> Stream a -> Stream d
snapshot3Lambda f = runFn5 snapshot3LambdaImpl (mkFn3 f)

snapshot4Lambda :: forall a b c d e. (a -> b -> c -> d -> e) -> Array Dep -> Cell b -> Cell c -> Cell d -> Stream a -> Stream e
snapshot4Lambda f = runFn6 snapshot4LambdaImpl (mkFn4 f)

snapshot5Lambda :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Array Dep -> Cell b -> Cell c -> Cell d -> Cell e -> Stream a -> Stream f
snapshot5Lambda f = runFn7 snapshot5LambdaImpl (mkFn5 f)


snapshot6Lambda :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Array Dep -> Cell b -> Cell c -> Cell d -> Cell e -> Cell f -> Stream a -> Stream g
snapshot6Lambda f = runFn8 snapshot6LambdaImpl (mkFn6 f)


foreign import mapLambda1StreamImpl :: forall a b c. Fn3 (a -> b) (Array c) (Stream a) (Stream b)

foreign import snapshotLambdaImpl :: forall a b c. Fn4 (Fn2 a b c) (Array Dep) (Cell b) (Stream a) (Stream c)
foreign import snapshot3LambdaImpl :: forall a b c d. Fn5 (Fn3 a b c d) (Array Dep) (Cell b) (Cell c) (Stream a) (Stream d)
foreign import snapshot4LambdaImpl :: forall a b c d e. Fn6 (Fn4 a b c d e) (Array Dep) (Cell b) (Cell c) (Cell d) (Stream a) (Stream e)
foreign import snapshot5LambdaImpl :: forall a b c d e f. Fn7 (Fn5 a b c d e f) (Array Dep) (Cell b) (Cell c) (Cell d) (Cell e) (Stream a) (Stream f)
foreign import snapshot6LambdaImpl :: forall a b c d e f g. Fn8 (Fn6 a b c d e f g) (Array Dep) (Cell b) (Cell c) (Cell d) (Cell e) (Cell f) (Stream a) (Stream g)

