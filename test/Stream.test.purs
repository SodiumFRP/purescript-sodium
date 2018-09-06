module Test.Stream (testStream) where

import Prelude

import Effect.Class (liftEffect)
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
    collect,
    accum,
    once,
    execute
)

import SodiumFRP.Class (
    send, 
    listen, 
    newStreamSink, 
    newCellSink,
    newCell,
    StreamSink,
    toStream
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
            a <- liftEffect $ newStreamSink Nothing
            result <- makeAff \cb -> do
                unlisten <- listen a \value ->
                    cb $ Right value 
                send a 2
                unlisten
                pure nonCanceler 
            Assert.equal result 2
        test "single send with map" do
            a <- liftEffect $ newStreamSink Nothing
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send a 2
                unlisten
                pure nonCanceler 
            Assert.equal result 4

        test "multi send with map" do
            a <- liftEffect $ newStreamSink Nothing
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            results <- makeAff \cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send a 2
                send a 3
                unlisten
                pure nonCanceler 
            Assert.equal (fromFoldable [4, 6]) results

        test "mapTo" do
            a <- liftEffect $ newStreamSink Nothing
            let b = mapTo 4 a
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send a 2
                unlisten
                pure nonCanceler 
            Assert.equal result 4

    suite "[stream] merge tests" do
        test "merge constructor left" do
            a <- liftEffect $ newStreamSink (Just $ \l -> \r -> l)
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send a 2
                        send a 3
                )
                unlisten
                pure nonCanceler 
            Assert.equal (4) result
        test "merge constructor right" do
            a <- liftEffect $ newStreamSink (Just $ \l -> \r -> r)
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                runTransaction (
                    do 
                        send a 2
                        send a 3
                )
                unlisten
                pure nonCanceler 
            Assert.equal (6) result
        test "orElse" do
            a <- liftEffect $ newStreamSink Nothing 
            b <- liftEffect $ newStreamSink Nothing
            let c = orElse a b
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                runTransaction (
                    do 
                        send a 2
                        send b 3
                )
                unlisten
                pure nonCanceler 
            Assert.equal 2 result
        test "merge left" do
            a <- liftEffect $ newStreamSink Nothing 
            b <- liftEffect $ newStreamSink Nothing
            let c = merge (\l -> \r -> l) a b
            result <- makeAff \cb -> do
                unlisten <- listen c \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send a 2
                        send b 3
                )
                unlisten
                pure nonCanceler 
            Assert.equal 2 result
        test "merge right" do
            a <- liftEffect $ newStreamSink Nothing 
            b <- liftEffect $ newStreamSink Nothing
            let c = merge (\l -> \r -> r) a b
            result <- makeAff \cb -> do
                unlisten <- listen c \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send a 2
                        send b 3
                )
                unlisten
                pure nonCanceler 
            Assert.equal 3 result
    suite "[stream] filter" do
        test "filter" do
            a <- liftEffect $ newStreamSink Nothing
            let b = filter (\x -> x == 2) a
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send a 4
                send a 3
                send a 2
                unlisten
                pure nonCanceler 
            Assert.equal result 2

    suite "[stream] gate" do
        test "gate" do
            a <- liftEffect $ newStreamSink Nothing
            b <- liftEffect $ newCellSink false Nothing
            let c = gate a b
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send a 4
                send a 3
                send b true
                send a 2
                unlisten
                pure nonCanceler 
            Assert.equal result 2
    
    suite "[stream] snapshot" do
        test "snapshot1" do
            a <- liftEffect $ newStreamSink Nothing
            let b = newCell 2
            let c = snapshot1 (a :: StreamSink Int) b
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send a 1
                unlisten
                pure nonCanceler 
            Assert.equal result 2
        test "snapshot" do
            a <- liftEffect $ newStreamSink Nothing
            let b = newCell 2
            let c = snapshot (\x1 -> \x2 -> x1 + x2) (a :: StreamSink Int) b
            result <- makeAff \cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                send a 1
                unlisten
                pure nonCanceler 
            Assert.equal result 3
        test "snapshot3" do
            a <- liftEffect $ newStreamSink Nothing
            let b = newCell 2
            let c = newCell 3
            let d = snapshot3 
                    (\x1 -> \x2 -> \x3 -> x1 + x2 + x3) 
                    a b c 
            result <- makeAff \cb -> do
                unlisten <- listen d \value ->
                    cb $ Right value 
                send a 1
                unlisten
                pure nonCanceler 
            Assert.equal result 6
        test "snapshot4" do
            a <- liftEffect $ newStreamSink Nothing
            let b = newCell 2
            let c = newCell 3
            let d = newCell 4
            let e = snapshot4 
                    (\x1 -> \x2 -> \x3 -> \x4 -> x1 + x2 + x3 + x4) 
                    a b c d
            result <- makeAff \cb -> do
                unlisten <- listen e \value ->
                    cb $ Right value 
                send a 1
                unlisten
                pure nonCanceler 
            Assert.equal result 10 
        test "snapshot5" do
            a <- liftEffect $ newStreamSink Nothing
            let b = newCell 2
            let c = newCell 3
            let d = newCell 4
            let e = newCell 5
            let f = snapshot5 
                    (\x1 -> \x2 -> \x3 -> \x4 -> \x5 ->
                        x1 + x2 + x3 + x4 + x5
                    ) 
                    a b c d e
            result <- makeAff \cb -> do
                unlisten <- listen f \value ->
                    cb $ Right value 
                send a 1
                unlisten
                pure nonCanceler 
            Assert.equal result 15 
        test "snapshot6" do
            a <- liftEffect $ newStreamSink Nothing
            let b = newCell 2
            let c = newCell 3
            let d = newCell 4
            let e = newCell 5
            let f = newCell 6
            let g = snapshot6 
                    (\x1 -> \x2 -> \x3 -> \x4 -> \x5 -> \x6 -> 
                        x1 + x2 + x3 + x4 + x5 + x6
                    ) 
                    a b c d e f
            result <- makeAff \cb -> do
                unlisten <- listen g \value ->
                    cb $ Right value 
                send a 1
                unlisten
                pure nonCanceler 
            Assert.equal result 21 
    suite "[stream] hold" do
        test "hold" do
            a <- liftEffect $ newStreamSink Nothing
            b <- liftEffect $ hold a 2
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                unlisten
                pure nonCanceler 
            Assert.equal result 2
    suite "[stream] collect" do
        test "collect - one round" do
            a <- liftEffect $ newStreamSink Nothing
            let b = collect
                    (\x -> \state -> {value: x + state, state: state + 1})
                    1 a
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send a 1
                unlisten
                pure nonCanceler 
            Assert.equal result 2 

        test "collect - multi round" do
            a <- liftEffect $ newStreamSink Nothing
            let b = collect (\x -> \state -> {value: x + state, state: state + 1}) 1 a 
            results <- makeAff \cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send a 1
                send a 1
                unlisten
                pure nonCanceler 
            Assert.equal (fromFoldable [2, 3]) results
    
    suite "[stream] accum" do
        test "accum - one round" do
            a <- liftEffect $ newStreamSink Nothing
            let b = accum
                    (\x -> \state -> state + x)
                    1 a
            results <- makeAff \cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send a 1
                unlisten
                pure nonCanceler 

            Assert.equal (fromFoldable [1, 2]) results
        test "accum - multi round" do
            a <- liftEffect $ newStreamSink Nothing
            let b = accum
                    (\x -> \state -> state + x)
                    1 a
            results <- makeAff \cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 3) then (cb $ Right xs) else (pure unit)
                send a 1
                send a 1
                unlisten
                pure nonCanceler 

            Assert.equal (fromFoldable [1, 2, 3]) results
    suite "[stream] once" do
        test "once" do
            a <- liftEffect $ newStreamSink Nothing
            let b = once a 
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send a 2
                unlisten
                pure nonCanceler 
            Assert.equal result 2
    suite "[stream] execute" do
        test "execute" do
            a <- liftEffect $ ((newStreamSink Nothing) :: Effect (StreamSink (Effect String)))
            let b = execute a
            result <- makeAff \cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send a (mockDbGet)
                unlisten
                pure nonCanceler 
            Assert.equal result "DB RESULT" 

foreign import mockDbGet :: Effect String
