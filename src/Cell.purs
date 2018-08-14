module SodiumFRP.Cell (
    Cell,
    CellSink,
    newCell,
    newCellSink
) where

import SodiumFRP.Stream (Stream)

import Prelude
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)
import Data.Function.Uncurried (Fn2, runFn2, mkFn2, Fn3, runFn3)

-- Cell
data Cell a = Cell a

newCell :: forall a. a -> Maybe (Stream a) -> Cell a
newCell a s = runFn2 newCellImpl a (toNullable s)

-- Cell Sink
data CellSink a = CellSink a

newCellSink :: forall a. a -> Maybe (a -> a -> a) -> CellSink a
newCellSink a m = runFn2 newCellSinkImpl a (toNullable (mkFn2 <$> m))

toCell :: forall a. CellSink a -> Cell a
toCell = toCellImpl

-- Foreign imports : Cell
foreign import newCellImpl :: forall a. Fn2 a (Nullable (Stream a)) (Cell a)

-- Foreign imports : CellSink
foreign import newCellSinkImpl :: forall a. Fn2 a (Nullable (Fn2 a a a)) (CellSink a)
foreign import toCellImpl :: forall a. CellSink a -> Cell a
