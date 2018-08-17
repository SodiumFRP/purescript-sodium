module Test.Lambda (testLambda) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)

import SodiumFRP.Lambda (
    mapLambda1
)
import SodiumFRP.Cell (sample)

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
        test "single send with map" do
            let a = newStreamSink Nothing
            let c = newCell 2 Nothing
            let b = mapLambda1 
                        ((\x -> x + (sample c)) :: Int -> Int) 
                        [c]
                        (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send 2 a
                unlisten
                pure nonCanceler 
            Assert.equal result 4
