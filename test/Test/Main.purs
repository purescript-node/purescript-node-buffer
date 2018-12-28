module Test.Main where

import Prelude
import Effect (Effect)
import Test.Node.Buffer as Buffer
import Test.Node.Buffer.Mutable as Mutable

main :: Effect Unit
main = do
  Buffer.test
  Mutable.test
