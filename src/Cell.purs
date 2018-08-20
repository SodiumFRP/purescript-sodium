module SodiumFRP.Cell (
    sample,
    loop
) where

import SodiumFRP.Class(
    Cell,
    CellLoop,
    class SodiumCell,
    toCell
)

import Prelude (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

-- Cell

sample :: forall a c. (SodiumCell c) => c a -> a
sample c = sampleImpl (toCell c)

foreign import sampleImpl :: forall a. Cell a -> a
{-|
    Resolve the loop to specify what the CellLoop was a forward reference to. 
    It must be invoked inside the same transaction as the place where the CellLoop is used.
    This requires you to create an explicit transaction 
-}
loop :: forall a c. (SodiumCell c) => c a -> CellLoop a -> Effect Unit
loop c = runEffectFn2 loopCellImpl (toCell c)

foreign import loopCellImpl :: forall a. EffectFn2 (Cell a) (CellLoop a) Unit
