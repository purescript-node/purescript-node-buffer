module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)
import Test.Assert

import Node.Buffer

main :: Eff _ Unit
main = do
  log "Testing..."
  log "Reading and writing"
  testReadWrite
  log "fromArray"
  testFromArray
  log "toArray"
  testToArray

testReadWrite :: Eff _ Unit
testReadWrite = do
  buf <- create 1
  let val = 42
  write UInt8 val 0 buf
  readVal <- read UInt8 0 buf

  assertEq val readVal

testFromArray :: Eff _ Unit
testFromArray = do
  buf <- fromArray [1,2,3,4,5]
  readVal <- read UInt8 2 buf

  assertEq 3 readVal

testToArray :: Eff _ Unit
testToArray = do
  let val = [1,2,67,3,3,7,8,3,4,237]

  buf    <- fromArray val
  valOut <- toArray buf

  assertEq val valOut

assertEq :: forall a. (Eq a, Show a) => a -> a -> Eff _ Unit
assertEq x y =
  if x == y
    then return unit
    else let msg = show x ++ " and " ++ show y ++ " were not equal."
         in assert' msg false
