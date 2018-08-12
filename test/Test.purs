module Test.Main where

import Prelude
import Effect (Effect)
import Test.Stream (testStream)
import Test.Transaction (testTransaction)

main :: Effect Unit
main = do
    _ <- testStream
    _ <- testTransaction
    pure unit
