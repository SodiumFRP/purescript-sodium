module Test.Categories (testCategories) where

import Prelude

import SodiumFRP.Class(Stream, Cell, newStream)

import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)

import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck.Laws (A, B, C, checkLaws)
import Test.QuickCheck (Result(), (===))
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Type.Proxy (Proxy2 (..))

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

testCategories :: Effect Unit
testCategories = runTest do
    suite "[categories]" do
        test "[stream] functor" do
           liftEffect $ checkFunctor prxStream 
        where prxStream = Proxy2 :: Proxy2 (Cell)
