module Test.Operational (testOperational) where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, launchAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.List (List(Nil), snoc, length, fromFoldable)

import SodiumFRP.Operational (
    updates,
    value,
    defer,
    split
)
import SodiumFRP.Class (
    listen, 
    newStreamSink, 
    newCellSink,
    send
)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import SodiumFRP.Transaction (runTransaction)

testOperational :: Effect Unit
testOperational = runTest do
    suite "[updates]" do
        test "updates" do
            a <- liftEffect $ newCellSink 2 Nothing
            let b = updates a
            results <- makeAff \cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \v -> do
                    Ref.modify_ (\xs -> snoc xs v) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send a 2
                send a 3
                unlisten
                pure nonCanceler 
            Assert.equal (fromFoldable [2, 3]) results
        test "value" do
            results <- makeAff \cb1 -> do 
                a <- liftEffect $ newCellSink 2 Nothing
                aff <- runTransaction (do
                    _ <- launchAff $ makeAff \cb2 -> do
                        b <- value a
                        refList <- Ref.new (Nil :: List Int)
                        unlisten <- listen b \v -> do
                            Ref.modify_ (\xs -> snoc xs v) refList
                            xs <- Ref.read refList
                            if (length xs == 2) then (cb1 $ Right xs) else (pure unit)
                        pure nonCanceler
                    pure unit 
                )
                send a 3
                pure nonCanceler
            Assert.equal (fromFoldable [2, 3]) results
        test "defer" do
            a <- liftEffect $ newStreamSink Nothing
            let b = defer a
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
            Assert.equal (fromFoldable [2, 3]) results
        test "split" do
            a <- liftEffect $ newStreamSink Nothing
            let b = split a
            results <- makeAff \cb -> do
                refList <- Ref.new (Nil :: List Int)
                unlisten <- listen b \value -> do
                    Ref.modify_ (\xs -> snoc xs value) refList
                    xs <- Ref.read refList
                    if (length xs == 2) then (cb $ Right xs) else (pure unit)
                send a [2,3] 
                unlisten
                pure nonCanceler 
            Assert.equal (fromFoldable [2, 3]) results
