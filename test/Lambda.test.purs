module Test.Lambda (testLambda) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Class (liftEffect)

import SodiumFRP.Lambda (
    mapLambda1,
    snapshotLambda,
    mkDep
)
import SodiumFRP.Cell (sample)
--import SodiumFRP.Stream (snapshot)
import SodiumFRP.Class (
    listen, 
    newStreamSink, 
    newCell,
    send,
    toStream
)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testLambda :: Effect Unit
testLambda = runTest do
    suite "[lambda] basic tests" do
        test "map w/ lambda1" do
            a <- liftEffect $ newStreamSink Nothing
            b <- liftEffect $ newCell 2 Nothing
            let c = mapLambda1 
                        ((\x -> x + (sample b)) :: Int -> Int) 
                        [mkDep a, mkDep b]
                        (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send 3 a
                unlisten
                pure nonCanceler 
            Assert.equal result 5
        test "snapshot w/ lambda2" do
            a <- liftEffect $ newStreamSink Nothing
            b <- liftEffect $ newCell 2 Nothing
            let c = snapshotLambda
                        ((\x -> \y -> x + y + (sample b))) 
                        [mkDep a, mkDep b]
                        (b)
                        (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send 3 a
                unlisten
                pure nonCanceler 
            Assert.equal result 7

