module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console (log, CONSOLE())
import Test.Assert

import Node.Buffer
import Node.Encoding

type Test = forall e. Eff (assert :: ASSERT, buffer :: BUFFER, console :: CONSOLE | e) Unit

main :: Test
main = do
  log "Testing..."

  log "Reading and writing"
  testReadWrite

  log "fromArray"
  testFromArray

  log "toArray"
  testToArray

  log "fromString"
  testFromString

  log "toString"
  testToString

  log "readString"
  testReadString

testReadWrite :: Test
testReadWrite = do
  buf <- create 1
  let val = 42
  write UInt8 val 0 buf
  readVal <- read UInt8 0 buf

  assertEq val readVal

testFromArray :: Test
testFromArray = do
  buf <- fromArray [1,2,3,4,5]
  readVal <- read UInt8 2 buf

  assertEq 3 readVal

testToArray :: Test
testToArray = do
  let val = [1,2,67,3,3,7,8,3,4,237]

  buf    <- fromArray val
  valOut <- toArray buf

  assertEq val valOut

testFromString :: Test
testFromString = do
  let str = "hello, world"

  buf <- fromString str ASCII
  val <- read UInt8 6 buf

  assertEq val 32 -- ASCII space

testToString :: Test
testToString = do
  let str = "hello, world"

  buf    <- fromString str ASCII
  strOut <- toString ASCII buf

  assertEq str strOut

testReadString :: Test
testReadString = do
  let str = "hello, world"

  buf    <- fromString str ASCII
  strOut <- readString ASCII 7 12 buf

  assertEq "world" strOut

assertEq :: forall a. (Eq a, Show a) => a -> a -> Test
assertEq x y =
  if x == y
    then return unit
    else let msg = show x ++ " and " ++ show y ++ " were not equal."
         in assert' msg false
