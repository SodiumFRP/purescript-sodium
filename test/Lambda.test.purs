module Test.Lambda (testLambda) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Unsafe(unsafePerformEffect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Class (liftEffect)

import SodiumFRP.Lambda (
    mapLambda1,
    snapshotLambda,
    liftLambda,
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
            let b = newCell 2
            let c = mapLambda1 
                        ((\x -> x + (unsafePerformEffect $ sample b)) :: Int -> Int) 
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
            let b = newCell 2
            let c = snapshotLambda
                        ((\x -> \y -> x + y + (unsafePerformEffect $ sample b))) 
                        [mkDep a, mkDep b]
                        (b)
                        a 
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send 3 a
                unlisten
                pure nonCanceler 
            Assert.equal result 7
        test "lift w/ lambda2" do
            let b = newCell 2
            let c = liftLambda
                        ((\x y -> x + y + (unsafePerformEffect $ sample b))) 
                        [mkDep b]
                        (b)
                        (newCell 3)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 7

