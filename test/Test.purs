module Test.Main where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import FRP.Sodium (newStreamSink, listen, send)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Effect.Ref as Ref
import Data.List (List(Nil), snoc, length, fromFoldable)

main :: Effect Unit
main = runTest do
    suite "basic stream tests" do
        test "test single send" do
            let a = newStreamSink Nothing
            let b = ((\x -> x + x) :: Int -> Int) <$> a
            result <- makeAff (\cb -> do
                unlisten <- listen b \value ->
                    cb $ Right value 
                send a 2
                unlisten
                pure nonCanceler 
            )
            Assert.equal result 4

        test "test multi send" do
            let a = newStreamSink Nothing
            let b = ((\x -> x + x) :: Int -> Int) <$> a
            results <- makeAff (\cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send a 2
                send a 3
                unlisten
                pure nonCanceler 
            )
            Assert.equal results (fromFoldable [4, 6])
