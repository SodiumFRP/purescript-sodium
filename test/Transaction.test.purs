module Test.Transaction (testTransaction) where

import Prelude

import SodiumFRP.Transaction (runTransaction)
import SodiumFRP.Class (newCellLoop, newCell, newStream, newStreamLoop, loop2)
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

        test "cell stream loop" do
            result <- liftEffect $ do
                cc <- loop2 (\ret ca' cb' -> do
                    let cc = (_ * 2) <$> cb'
                    let cb = (_ * 2) <$> ca'
                    let ca = newCell 2
                    pure $ ret ca cb cc
                )
                pure $ sample cc
            Assert.equal result 8

        test "cell loop in transaction" do
            result <- liftEffect $ runTransaction (do
                l <- newCellLoop
                let c = newCell 2
                loopCell c l
                pure $ sample l
            )
            Assert.equal result 2

        test "stream loop in transaction" do
            result <- liftEffect $ runTransaction (do
                l <- newStreamLoop
                let s = newStream
                loopStream s l
                pure unit
            )
            pure unit
