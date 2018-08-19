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

data Lambda f = Lambda f (Array Dep)

lambda1 :: forall a b. (a -> b) -> Array Dep -> Lambda (a -> b)
lambda1 = Lambda

foreign import sodiumLambda1Impl :: forall a b. Fn2 (a -> b) (Array SodiumObject) (SodiumLambda1 a b)
foreign import sodiumStreamMapImpl :: forall a b. Fn2 (SodiumLambda1 a b) (Stream a) (Stream b)
foreign import sodiumCellMapImpl :: forall a b. Fn2 (SodiumLambda1 a b) (Cell a) (Cell b)

sodiumLambda1 :: forall lam a b l. Lambda1 lam a b => lam -> (SodiumLambda1 a b)
sodiumLambda1 lam = runFn2 sodiumLambda1Impl (apply lam) (unwrapDeps $ deps lam)

class Lambda1 lam a b | lam -> a, lam -> b where
    apply :: lam -> a -> b
    deps :: lam -> Array Dep

instance lambda1Lambda :: Lambda1 (Lambda (a -> b)) a b where
    apply (Lambda f _) a = f a
    deps (Lambda _ deps') = deps'

instance lambda1Closure :: Lambda1 (a -> b) a b where
    apply f a = f a
    deps _ = mempty

class SodiumFunctor f where
    mapd :: forall lam a b. Lambda1 lam a b => lam -> f a -> f b

instance sodiumFunctorStream :: SodiumFunctor Stream where
    mapd f s = runFn2 sodiumStreamMapImpl (sodiumLambda1 f) s

instance sodiumFunctorCell :: SodiumFunctor Cell where
    mapd f c = runFn2 sodiumCellMapImpl (sodiumLambda1 f) c
