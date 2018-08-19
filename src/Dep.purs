module SodiumFRP.Dep where

import Data.Exists
import Data.Function (($))
import Data.Functor ((<$>))
import Unsafe.Coerce (unsafeCoerce)
import SodiumFRP.Class(
  Cell,
  Stream,
  CellSink,
  StreamSink
)

data Dep
  =
  DepCell (Exists Cell) |
  DepStream (Exists Stream) |
  DepCellSink (Exists CellSink) |
  DepStreamSink (Exists StreamSink)

data SodiumObject

unwrapDeps :: Array Dep -> Array SodiumObject
unwrapDeps deps =
  (\dep ->
      case dep of
        (DepCell a) -> runExists (\b -> (unsafeCoerce b) :: SodiumObject) a
        (DepStream a) -> runExists (\b -> (unsafeCoerce b) :: SodiumObject) a
        (DepCellSink a) -> runExists (\b -> (unsafeCoerce b) :: SodiumObject) a
        (DepStreamSink a) -> runExists (\b -> (unsafeCoerce b) :: SodiumObject) a
  ) <$> deps

class IsDep a where
  dep :: a -> Dep

instance isDepCell :: IsDep (Cell a) where
  dep a = DepCell $ mkExists a

instance isDepStream :: IsDep (Stream a) where
  dep a = DepStream $ mkExists a

instance isDepCellSink :: IsDep (CellSink a) where
  dep a = DepCellSink $ mkExists a

instance isDepStreamSink :: IsDep (StreamSink a) where
  dep a = DepStreamSink $ mkExists a
