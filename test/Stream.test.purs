module Test.Stream (testStream) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing, Just))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import SodiumFRP.Stream (
    newStreamSink, 
    send, 
    toStream,
    mapTo,
    orElse,
    merge,
    filter
)
import SodiumFRP.Class (listen)
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
            result <- makeAff (\cb -> do
                unlisten <- listen (toStream a) \value ->
                    cb $ Right value 
                send 2 a
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 2
        test "single send with map" do
            let a = newStreamSink Nothing
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff (\cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send 2 a
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 4

        test "multi send with map" do
            let a = newStreamSink Nothing
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            results <- makeAff (\cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send 2 a
                send 3 a
                unlisten
                pure nonCanceler 
            )
            Assert.equal (fromFoldable [4, 6]) results

        test "mapTo" do
            let a = newStreamSink Nothing
            let b = mapTo 4 (toStream a)
            result <- makeAff (\cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send 2 a
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 4

    suite "[stream] merge tests" do
        test "merge constructor left" do
            let a = newStreamSink (Just $ \l -> \r -> l)
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff (\cb -> do
                unlisten <- listen b \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 a
                )
                unlisten
                pure nonCanceler 
            )
            Assert.equal (4) result
        test "merge constructor right" do
            let a = newStreamSink (Just $ \l -> \r -> r)
            let b = ((\x -> x + x) :: Int -> Int) <$> (toStream a)
            result <- makeAff (\cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 a
                )
                unlisten
                pure nonCanceler 
            )
            Assert.equal (6) result
        test "orElse" do
            let a = newStreamSink Nothing 
            let b = newStreamSink Nothing
            let c = orElse (toStream a) (toStream b)
            result <- makeAff (\cb -> do
                unlisten <- listen c \value ->
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 b
                )
                unlisten
                pure nonCanceler 
            )
            Assert.equal 3 result
        test "merge left" do
            let a = newStreamSink Nothing
            let b = newStreamSink Nothing 
            let c = merge (\l -> \r -> l) (toStream a) (toStream b)
            result <- makeAff (\cb -> do
                unlisten <- listen c \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 b
                )
                unlisten
                pure nonCanceler 
            )
            Assert.equal 2 result
        test "merge right" do
            let a = newStreamSink Nothing
            let b = newStreamSink Nothing 
            let c = merge (\l -> \r -> r) (toStream a) (toStream b)
            result <- makeAff (\cb -> do
                unlisten <- listen c \value -> 
                    cb $ Right value 
                runTransaction (
                    do 
                        send 2 a
                        send 3 b
                )
                unlisten
                pure nonCanceler 
            )
            Assert.equal 3 result
    suite "[stream] filter" do
        test "filter" do
            let a = newStreamSink Nothing
            let b = filter (\x -> x == 2) (toStream a)
            result <- makeAff (\cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send 4 a
                send 3 a
                send 2 a
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 2
