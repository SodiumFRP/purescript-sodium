module SodiumFRP.Cell (
    sample,
    loop
) where

import SodiumFRP.Class(
    Cell,
    CellLoop
)

import Prelude (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

-- Cell

sample :: forall a. Cell a -> a
sample = sampleImpl

{-|
    Resolve the loop to specify what the CellLoop was a forward reference to. 
    It must be invoked inside the same transaction as the place where the CellLoop is used.
    This requires you to create an explicit transaction 
-}
loop :: forall a. Cell a -> CellLoop a -> Effect Unit
loop = runEffectFn2 loopCellImpl

-- Foreign imports
foreign import sampleImpl :: forall a. Cell a -> a

foreign import loopCellImpl :: forall a. EffectFn2 (Cell a) (CellLoop a) Unit
