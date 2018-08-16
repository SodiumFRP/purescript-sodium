module SodiumFRP.Cell (
    sample
) where

import SodiumFRP.Class(
    Cell
)

-- Cell

sample :: forall a. Cell a -> a
sample = sampleImpl

-- Foreign imports : Cell
foreign import sampleImpl :: forall a. Cell a -> a

-- Foreign imports : CellSink

