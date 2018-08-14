module Test.Main where

import Prelude
import Effect (Effect)
import Test.Stream (testStream)
import Test.Transaction (testTransaction)
import Test.Cell (testCell)

main :: Effect Unit
main = do
    testTransaction
    testStream
    testCell 
