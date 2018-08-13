module Test.Transaction (testTransaction) where

import Prelude

import SodiumFRP.Transaction (runTransaction)
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testTransaction :: Effect Unit
testTransaction = runTest do
    suite "Transaction" do
        test "test pure transaction" do
            result <- liftEffect $ runTransaction (
              pure 2
            )
            Assert.equal result 2
