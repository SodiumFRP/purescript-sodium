module Test.Categories (testCategories) where

import Prelude

import SodiumFRP.Class(Cell, newCell)
import SodiumFRP.Cell (sample)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Class (liftEffect)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Type.Proxy (Proxy2 (..))
import Test.QuickCheck.Arbitrary(class Arbitrary, arbitrary)



testCategories :: Effect Unit
testCategories = runTest do
    suite "[cell categories]" do
        test "[cell] functor" do
           liftEffect $ checkFunctor prxCell
        test "[cell] apply" do
           liftEffect $ checkApply prxCell
        test "[cell] applicative" do
           liftEffect $ checkApplicative prxCell

prxCell :: Proxy2 ArbitraryCell
prxCell = Proxy2

--don't want to import all the quickcheck stuff into core lib
--so we need to make a newtype and re-define instances for it
newtype ArbitraryCell a = ArbitraryCell (Cell a)

getCellFromArbitrary :: forall a. ArbitraryCell a -> Cell a
getCellFromArbitrary (ArbitraryCell c) = c

instance arbCell :: (Arbitrary a) => Arbitrary (ArbitraryCell a) where
  arbitrary = do
    a <- arbitrary
    pure $ ArbitraryCell (newCell a) 

instance eqArbitraryCell :: (Eq a) => Eq (ArbitraryCell a) where
    eq (ArbitraryCell cell1) (ArbitraryCell cell2) 
        = eq (unsafePerformEffect $ sample cell1) (unsafePerformEffect $ sample cell2)

instance functorArbitraryCell :: Functor ArbitraryCell where
    map func (ArbitraryCell a) = ArbitraryCell $ map func a

instance applyArbitraryCell :: Apply ArbitraryCell where
    apply (ArbitraryCell a) (ArbitraryCell b) = ArbitraryCell $ apply a b 

instance applicativeArbitraryCell :: Applicative ArbitraryCell where
    pure a = ArbitraryCell $ pure a 
