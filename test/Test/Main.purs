module Test.Main where

import Prelude
import Effect (Effect)
import Test.Node.Buffer as Buffer
import Test.Node.Buffer.Effect as Effect
import Test.Node.Buffer.ST as ST

main :: Effect Unit
main = do
  Buffer.test
  Effect.test
  ST.test
