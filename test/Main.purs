module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE())
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Node.Buffer (BUFFER, BufferValueType(..), toArray, concat', fromArray, fill, copy, readString, fromString, toString, read, write, create, getAtOffset)
import Node.Encoding (Encoding(..))
import Test.Assert (ASSERT, assert')

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

  log "copy"
  testCopy

  log "fill"
  testFill

  log "concat'"
  testConcat'

  log "getAtOffset"
  testGetAtOffset

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

testCopy :: Test
testCopy = do
  buf1 <- fromArray [1,2,3,4,5]
  buf2 <- fromArray [10,9,8,7,6]

  copied <- copy 0 3 buf1 2 buf2
  out    <- toArray buf2

  assertEq copied 3
  assertEq out [10,9,1,2,3]

testFill :: Test
testFill = do
  buf <- fromArray [1,1,1,1,1]
  fill 42 2 4 buf
  out <- toArray buf

  assertEq [1,1,42,42,1] out

testConcat' :: Test
testConcat' = do
  bufs <- traverse fromArray $ map (\x -> [x, x+1, x+2]) [0,3,6,9,12]
  buf  <- concat' bufs 15
  out  <- toArray buf

  assertEq [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14] out

testGetAtOffset :: Test
testGetAtOffset = do
  buf  <- fromArray [1, 2, 3, 4]
  assertEq (Just 2) =<< getAtOffset 1 buf
  assertEq Nothing  =<< getAtOffset 4 buf
  assertEq Nothing  =<< getAtOffset (-1) buf

assertEq :: forall a. Eq a => Show a => a -> a -> Test
assertEq x y =
  if x == y
    then pure unit
    else let msg = show x <> " and " <> show y <> " were not equal."
         in assert' msg false
