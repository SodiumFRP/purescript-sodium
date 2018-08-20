module Test.Transaction (testTransaction) where

import Prelude

import SodiumFRP.Transaction (runTransaction)
import SodiumFRP.Class (newCellLoop, newCell, toCell)
import SodiumFRP.Cell (loop, sample)

import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testTransaction :: Effect Unit
testTransaction = runTest do
    suite "[transaction]" do
        test "pure transaction" do
            result <- liftEffect $ runTransaction (
              pure 2
            )
            Assert.equal result 2

        test "loop in transaction" do
            result <- liftEffect $ runTransaction (do
                l <- newCellLoop
                let c = newCell 2 Nothing
                loop c l
                pure $ sample (toCell l)
            )
            Assert.equal result 2
