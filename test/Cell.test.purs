module Test.Cell (testCell) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing, Just))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import SodiumFRP.Cell (
    newCellSink, 
    newCell,
    toCell
)

import SodiumFRP.Multi (listen)
import SodiumFRP.Transaction (runTransaction)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Effect.Ref as Ref
import Data.List (List(Nil), snoc, length, fromFoldable)

testCell :: Effect Unit
testCell = runTest do
    suite "[cell] basic tests" do
        test "test constant" do
            let a = newCell 2 Nothing
            result <- makeAff (\cb -> do
                unlisten <- listen a \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 2
