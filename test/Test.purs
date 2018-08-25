module Test.Main where

import Prelude
import Effect (Effect)
import Test.Stream (testStream)
import Test.Transaction (testTransaction)
import Test.Cell (testCell)
import Test.Lambda (testLambda)
import Test.Categories (testCategories)

main :: Effect Unit
main = do
    testStream
    testCell 
    testLambda
    testTransaction
    testCategories
