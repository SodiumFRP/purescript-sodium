module Test.Cell (testCell) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import SodiumFRP.Cell (
    sample
)

import SodiumFRP.Class (listen, newCell)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testCell :: Effect Unit
testCell = runTest do
    suite "[cell] basic tests" do
        test "constant" do
            let a = newCell 2 Nothing
            result <- makeAff (\cb -> do
                unlisten <- listen a \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 2
        test "map" do
            let a = newCell 2 Nothing
            let b = (\x -> x + x) <$> a
            result <- makeAff (\cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 4

        test "sample" do
            let a = newCell 2 Nothing
            Assert.equal (sample a) 2
