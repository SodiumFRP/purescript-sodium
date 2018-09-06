module Test.Transaction (testTransaction) where

import Prelude

import SodiumFRP.Transaction (runTransaction)
import SodiumFRP.Class (newCellLoop, newCell, newStream, newStreamLoop)
import SodiumFRP.Cell (loopCell, sample)
import SodiumFRP.Stream (loopStream) 
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

        test "cell loop in transaction" do
            result <- liftEffect $ runTransaction (do
                l <- newCellLoop
                let c = newCell 2
                loopCell l c
                sample l 
            )
            Assert.equal result 2
        
        test "stream loop in transaction" do
            result <- liftEffect $ runTransaction (do
                l <- newStreamLoop
                let s = newStream 
                loopStream l s
                pure unit 
            )
            pure unit
