module Test.Stream (testStream) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing, Just))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import SodiumFRP.Stream (
    mapTo,
    orElse,
    merge,
    filter,
    gate,
    snapshot1,
    snapshot,
    snapshot3,
    snapshot4,
    snapshot5,
    snapshot6,
    hold,
    collect
)
import SodiumFRP.Class (
    send, 
    listen, 
    newStreamSink, 
    toStream,
    newCellSink,
    toCell,
    newCell,
    Stream
)

import SodiumFRP.Transaction (runTransaction)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Effect.Ref as Ref
import Data.List (List(Nil), snoc, length, fromFoldable)

testStream :: Effect Unit
testStream = runTest do
    suite "[stream] basic tests" do
        test "single send" do
            let a = newStreamSink Nothing
            result <- makeAff \cb -> do
                unlisten <- listen (toStream a) \value ->
                    cb $ Right value 
                send 2 a
                unlisten
                pure nonCanceler 
            Assert.equal result 2
        test "single send with map" do
            let a = newStreamSink Nothing
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send 2 a
                unlisten
                pure nonCanceler 
            Assert.equal result 4

        test "multi send with map" do
            let a = newStreamSink Nothing
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            results <- makeAff \cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send 2 a
                send 3 a
                unlisten
                pure nonCanceler 
            Assert.equal (fromFoldable [4, 6]) results

        test "mapTo" do
            let a = newStreamSink Nothing
            let b = mapTo 4 (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send 2 a
                unlisten
                pure nonCanceler 
            Assert.equal result 4

    suite "[stream] merge tests" do
        test "merge constructor left" do
            let a = newStreamSink (Just $ \l -> \r -> l)
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 a
                )
                unlisten
                pure nonCanceler 
            Assert.equal (4) result
        test "merge constructor right" do
            let a = newStreamSink (Just $ \l -> \r -> r)
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 a
                )
                unlisten
                pure nonCanceler 
            Assert.equal (6) result
        test "orElse" do
            let a = newStreamSink Nothing 
            let b = newStreamSink Nothing
            let c = orElse (toStream a) (toStream b)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 b
                )
                unlisten
                pure nonCanceler 
            Assert.equal 3 result
        test "merge left" do
            let a = newStreamSink Nothing
            let b = newStreamSink Nothing 
            let c = merge (\l -> \r -> l) (toStream a) (toStream b)
            result <- makeAff \cb -> do
                unlisten <- listen c \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 b
                )
                unlisten
                pure nonCanceler 
            Assert.equal 2 result
        test "merge right" do
            let a = newStreamSink Nothing
            let b = newStreamSink Nothing 
            let c = merge (\l -> \r -> r) (toStream a) (toStream b)
            result <- makeAff \cb -> do
                unlisten <- listen c \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 b
                )
                unlisten
                pure nonCanceler 
            Assert.equal 3 result
    suite "[stream] filter" do
        test "filter" do
            let a = newStreamSink Nothing
            let b = filter (\x -> x == 2) (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send 4 a
                send 3 a
                send 2 a
                unlisten
                pure nonCanceler 
            Assert.equal result 2

    suite "[stream] gate" do
        test "gate" do
            let a = newStreamSink Nothing
            let b = newCellSink false Nothing
            let c = gate (toCell b) (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send 4 a
                send 3 a
                send true b
                send 2 a
                unlisten
                pure nonCanceler 
            Assert.equal result 2
    
    suite "[stream] snapshot" do
        test "snapshot1" do
            let a = newStreamSink Nothing
            let b = newCell 2 Nothing
            let c = snapshot1 b ((toStream a) :: Stream Int)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send 1 a
                unlisten
                pure nonCanceler 
            Assert.equal result 2
        test "snapshot" do
            let a = newStreamSink Nothing
            let b = newCell 2 Nothing
            let c = snapshot (\x1 -> \x2 -> x1 + x2) b ((toStream a) :: Stream Int)
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send 1 a
                unlisten
                pure nonCanceler 
            Assert.equal result 3
        test "snapshot3" do
            let a = newStreamSink Nothing
            let b = newCell 2 Nothing
            let c = newCell 3 Nothing
            let d = snapshot3 
                    (\x1 -> \x2 -> \x3 -> x1 + x2 + x3) 
                    b c 
                    ((toStream a) :: Stream Int)
            result <- makeAff \cb -> do
                unlisten <- listen d \value ->
                    cb $ Right value 
                send 1 a
                unlisten
                pure nonCanceler 
            Assert.equal result 6
        test "snapshot4" do
            let a = newStreamSink Nothing
            let b = newCell 2 Nothing
            let c = newCell 3 Nothing
            let d = newCell 4 Nothing
            let e = snapshot4 
                    (\x1 -> \x2 -> \x3 -> \x4 -> x1 + x2 + x3 + x4) 
                    b c d
                    ((toStream a) :: Stream Int)
            result <- makeAff \cb -> do
                unlisten <- listen e \value ->
                    cb $ Right value 
                send 1 a
                unlisten
                pure nonCanceler 
            Assert.equal result 10 
        test "snapshot5" do
            let a = newStreamSink Nothing
            let b = newCell 2 Nothing
            let c = newCell 3 Nothing
            let d = newCell 4 Nothing
            let e = newCell 5 Nothing
            let f = snapshot5 
                    (\x1 -> \x2 -> \x3 -> \x4 -> \x5 ->
                        x1 + x2 + x3 + x4 + x5
                    ) 
                    b c d e 
                    ((toStream a) :: Stream Int)
            result <- makeAff \cb -> do
                unlisten <- listen f \value ->
                    cb $ Right value 
                send 1 a
                unlisten
                pure nonCanceler 
            Assert.equal result 15 
        test "snapshot6" do
            let a = newStreamSink Nothing
            let b = newCell 2 Nothing
            let c = newCell 3 Nothing
            let d = newCell 4 Nothing
            let e = newCell 5 Nothing
            let f = newCell 6 Nothing
            let g = snapshot6 
                    (\x1 -> \x2 -> \x3 -> \x4 -> \x5 -> \x6 -> 
                        x1 + x2 + x3 + x4 + x5 + x6
                    ) 
                    b c d e f
                    ((toStream a) :: Stream Int)
            result <- makeAff \cb -> do
                unlisten <- listen g \value ->
                    cb $ Right value 
                send 1 a
                unlisten
                pure nonCanceler 
            Assert.equal result 21 
    suite "[stream] hold" do
        test "hold" do
            let a = (toStream $ newStreamSink Nothing) :: Stream Int
            let b = hold 2 a 
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 2
    suite "[stream] collect" do
        test "collect - one round" do
            let a = newStreamSink Nothing
            let b = collect
                    (\x -> \state -> {value: x + state, state: state + 1})
                    1 
                    (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send 1 a
                unlisten
                pure nonCanceler 
            Assert.equal result 2 

        test "collect - multi round" do
            let a = newStreamSink Nothing
            let b = collect (\x -> \state -> {value: x + state, state: state + 1}) 1 (toStream a)
            results <- makeAff \cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send 1 a
                send 1 a
                unlisten
                pure nonCanceler 
            Assert.equal (fromFoldable [2, 3]) results
