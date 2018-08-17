module Test.Main where

import Prelude
import Effect (Effect)
import Test.Stream (testStream)
import Test.Transaction (testTransaction)
import Test.Cell (testCell)
import Test.Lambda (testLambda)

main :: Effect Unit
main = do
    testTransaction
    testStream
    testCell 
    testLambda
