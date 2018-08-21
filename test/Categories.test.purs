module Test.Categories (testCategories) where

import Prelude

import SodiumFRP.Class (
    listen, 
    newCell,
    newCellSink,
    send
)

import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(), (===))

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

testCategories :: Effect Unit
testCategories = runTest do
    suite "[categories]" do
        test "[stream] functor" do
            quickCheck theCommutativeProperty
