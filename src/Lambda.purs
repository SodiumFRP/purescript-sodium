module SodiumFRP.Lambda where

import SodiumFRP.Class(Stream)

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

-- | Lambda1
class Lambda1 target where
    mapLambda1 :: forall a b c. (a -> b) -> Array c -> target a -> target b

-- | Stream
instance lambda1Stream :: Lambda1 Stream where
    mapLambda1 = runFn3 mapLambda1StreamImpl


foreign import mapLambda1StreamImpl :: forall a b c. Fn3 (a -> b) (Array c) (Stream a) (Stream b)
