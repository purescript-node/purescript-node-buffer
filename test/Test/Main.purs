module Test.Main where

import Prelude
import Effect (Effect)
import Test.Node.Buffer as Buffer
import Test.Node.Buffer.Immutable as Immutable

main :: Effect Unit
main = do
  Buffer.test
  Immutable.test
