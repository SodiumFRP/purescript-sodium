module Test.Cell (testCell) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (makeAff, nonCanceler)

import Effect.Ref as Ref
import Data.List (List(Nil), snoc, length, fromFoldable)
import SodiumFRP.Cell (
    sample
)

import SodiumFRP.Class (
    listen, 
    newCell,
    newCellSink,
    send
)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testCell :: Effect Unit
testCell = runTest do
    suite "[cell] basic tests" do
        test "constant" do
            a <- liftEffect $ newCell 2 Nothing
            result <- makeAff (\cb -> do
                unlisten <- listen a \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 2
        test "map" do
            a <- liftEffect $ newCell 2 Nothing
            let b = (\x -> x + x) <$> a
            result <- makeAff (\cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 4

        test "sink" do
            a <- liftEffect $ newCellSink 2 Nothing
            results <- makeAff (\cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen a \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send 4 a
                unlisten
                pure nonCanceler 
            )
            Assert.equal (fromFoldable [2, 4]) results


        test "sample" do
            a <- liftEffect $ newCell 2 Nothing
            Assert.equal (sample a) 2
