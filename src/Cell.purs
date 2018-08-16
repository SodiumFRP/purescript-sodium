module SodiumFRP.Cell (
    newCell,
    newCellSink,
    toCell,
    sample
) where

import SodiumFRP.Class(
    Cell,
    CellSink,
    Stream
)
import Prelude
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried (Fn2, runFn2, mkFn2)

-- Cell


-- | Create a new Cell
newCell :: forall a. a -> Maybe (Stream a) -> Cell a
newCell a s = runFn2 newCellImpl a (toNullable s)

sample :: forall a. Cell a -> a
sample = sampleImpl

-- Cell Sink

newCellSink :: forall a. a -> Maybe (a -> a -> a) -> CellSink a
newCellSink a m = runFn2 newCellSinkImpl a (toNullable (mkFn2 <$> m))

toCell :: forall a. CellSink a -> Cell a
toCell = toCellImpl

-- Foreign imports : Cell
foreign import newCellImpl :: forall a. Fn2 a (Nullable (Stream a)) (Cell a)
foreign import sampleImpl :: forall a. Cell a -> a

-- Foreign imports : CellSink
foreign import newCellSinkImpl :: forall a. Fn2 a (Nullable (Fn2 a a a)) (CellSink a)
foreign import toCellImpl :: forall a. CellSink a -> Cell a
