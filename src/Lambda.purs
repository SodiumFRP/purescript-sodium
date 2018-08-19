module SodiumFRP.Lambda where

import SodiumFRP.Class(Cell,Stream)

import SodiumFRP.Dep(Dep, unwrapDeps, SodiumObject)

import Prelude
import Effect (Effect)
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)

import Data.Function.Uncurried (
    Fn2, runFn2, mkFn2, 
    Fn3, runFn3, mkFn3, 
    Fn4, runFn4, mkFn4, 
    Fn5, runFn5, mkFn5,
    Fn6, runFn6, mkFn6,
    Fn7, runFn7
)
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)

data SodiumLambda1 a b
data SodiumLambda2 a b c
data SodiumLambda3 a b c d
data SodiumLambda4 a b c d e
data SodiumLambda5 a b c d e f
data SodiumLambda6 a b c d e f g

data Lambda f = Lambda f (Array Dep)

lambda1 :: forall a b. (a -> b) -> Array Dep -> Lambda (a -> b)
lambda1 = Lambda

foreign import sodiumLambda1Impl :: forall a b. Fn2 (a -> b) (Array SodiumObject) (SodiumLambda1 a b)
foreign import sodiumLambda2Impl :: forall a b c. Fn2 (Fn2 a b c) (Array SodiumObject) (SodiumLambda2 a b c)
foreign import sodiumLambda3Impl :: forall a b c d. Fn2 (Fn3 a b c d) (Array SodiumObject) (SodiumLambda3 a b c d)
foreign import sodiumLambda4Impl :: forall a b c d e. Fn2 (Fn4 a b c d e) (Array SodiumObject) (SodiumLambda4 a b c d e)
foreign import sodiumLambda5Impl :: forall a b c d e f. Fn2 (Fn5 a b c d e f) (Array SodiumObject) (SodiumLambda5 a b c d e f)
foreign import sodiumLambda6Impl :: forall a b c d e f g. Fn2 (Fn6 a b c d e f g) (Array SodiumObject) (SodiumLambda6 a b c d e f g)
foreign import sodiumStreamMapImpl :: forall a b. Fn2 (SodiumLambda1 a b) (Stream a) (Stream b)
foreign import sodiumCellMapImpl :: forall a b. Fn2 (SodiumLambda1 a b) (Cell a) (Cell b)

sodiumLambda1 :: forall lam a b. Lambda1 lam a b => lam -> (SodiumLambda1 a b)
sodiumLambda1 lam = runFn2 sodiumLambda1Impl (apply lam) (unwrapDeps $ deps lam)

class HasDeps lam where
    deps :: lam -> Array Dep

class HasDeps lam <= Lambda1 lam a b | lam -> a, lam -> b where
    apply :: lam -> a -> b

class HasDeps lam <= Lambda2 lam a b c | lam -> a, lam -> b, lam -> c where
    apply2 :: lam -> a -> b -> c

class HasDeps lam <= Lambda3 lam a b c d | lam -> a, lam -> b, lam -> c, lam -> d where
    apply3 :: lam -> a -> b -> c -> d

class HasDeps lam <= Lambda4 lam a b c d e | lam -> a, lam -> b, lam -> c, lam -> d, lam -> e where
    apply4 :: lam -> a -> b -> c -> d -> e

class HasDeps lam <= Lambda5 lam a b c d e f | lam -> a, lam -> b, lam -> c, lam -> d, lam -> e, lam -> f where
    apply5 :: lam -> a -> b -> c -> d -> e -> f

class HasDeps lam <= Lambda6 lam a b c d e f g | lam -> a, lam -> b, lam -> c, lam -> d, lam -> e, lam -> f, lam -> g where
    apply6 :: lam -> a -> b -> c -> d -> e -> f -> g

instance hasDepsLambda :: HasDeps (Lambda l) where
    deps (Lambda _ deps') = deps'

instance hasDepsFn :: HasDeps (a -> b) where
    deps _ = mempty

instance lambda1Lambda :: Lambda1 (Lambda (a -> b)) a b where
    apply (Lambda f _) = f

instance lambda1Closure :: Lambda1 (a -> b) a b where
    apply f a = f a

instance lambda2Lambda :: Lambda2 (Lambda (a -> b -> c)) a b c where
    apply2 (Lambda f _) = f

instance lambda2Fn :: Lambda2 (a -> b -> c) a b c where
    apply2 = identity

instance lambda3Lambda :: Lambda3 (Lambda (a -> b -> c -> d)) a b c d where
    apply3 (Lambda f _) = f

instance lambda3Fn :: Lambda3 (a -> b -> c -> d) a b c d where
    apply3 = identity

instance lambda4Lambda :: Lambda4 (Lambda (a -> b -> c -> d -> e)) a b c d e where
    apply4 (Lambda f _) = f

instance lambda4Fn :: Lambda4 (a -> b -> c -> d -> e) a b c d e where
    apply4 = identity

instance lambda5Lambda :: Lambda5 (Lambda (a -> b -> c -> d -> e -> f)) a b c d e f where
    apply5 (Lambda f _) = f

instance lambda5Fn :: Lambda5 (a -> b -> c -> d -> e -> f) a b c d e f where
    apply5 = identity

instance lambda6Lambda :: Lambda6 (Lambda (a -> b -> c -> d -> e -> f -> g)) a b c d e f g where
    apply6 (Lambda f _) = f

instance lambda6Fn :: Lambda6 (a -> b -> c -> d -> e -> f -> g) a b c d e f g where
    apply6 = identity

class SodiumFunctor f where
    mapd :: forall lam a b. Lambda1 lam a b => lam -> f a -> f b

instance sodiumFunctorStream :: SodiumFunctor Stream where
    mapd f s = runFn2 sodiumStreamMapImpl (sodiumLambda1 f) s

instance sodiumFunctorCell :: SodiumFunctor Cell where
    mapd f c = runFn2 sodiumCellMapImpl (sodiumLambda1 f) c
