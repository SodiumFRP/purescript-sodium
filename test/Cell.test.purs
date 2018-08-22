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
    sample,
    lift,
    lift3,
    lift4,
    lift5,
    lift6,
    switchC,
    switchS
)

import SodiumFRP.Class (
    listen, 
    newCell,
    newCellSink,
    send,
    newStreamSink,
    toStream
)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

testCell :: Effect Unit
testCell = runTest do
    suite "[cell] basic tests" do
        test "constant" do
            let a = newCell 2
            result <- makeAff (\cb -> do
                unlisten <- listen a \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 2
        test "map" do
            let a = newCell 2
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
            let a = newCell 2
            Assert.equal (sample a) 2
    suite "[cell] lift" do
        test "lift" do
            let c = lift
                    (\x1 -> \x2 -> x1 + x2)
                    (newCell 2)
                    (newCell 1)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 3
        test "lift3" do
            let c = lift3
                    (\x1 x2 x3 -> x1 + x2 + x3)
                    (newCell 3)
                    (newCell 2)
                    (newCell 1)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 6
        test "lift4" do
            let c = lift4
                    (\x1 x2 x3 x4 -> x1 + x2 + x3 + x4)
                    (newCell 4)
                    (newCell 3)
                    (newCell 2)
                    (newCell 1)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 10
        test "lift5" do
            let c = lift5
                    (\x1 x2 x3 x4 x5 -> x1 + x2 + x3 + x4 + x5)
                    (newCell 5)
                    (newCell 4)
                    (newCell 3)
                    (newCell 2)
                    (newCell 1)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 15
        test "lift6" do
            let c = lift6
                    (\x1 x2 x3 x4 x5 x6 -> x1 + x2 + x3 + x4 + x5 + x6)
                    (newCell 6)
                    (newCell 5)
                    (newCell 4)
                    (newCell 3)
                    (newCell 2)
                    (newCell 1)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 21 
    suite "[cell] switch" do
        test "switchC" do
            let c = switchC $ newCell (newCell 2)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 2
        test "switchS" do
            s1 <- liftEffect $ newStreamSink Nothing
            let s2 = switchS $ newCell (toStream s1)
            result <- makeAff \cb -> do
                unlisten <- listen s2 \value ->
                    cb $ Right value 
                send 2 s1
                unlisten
                pure nonCanceler 
            Assert.equal result 2
