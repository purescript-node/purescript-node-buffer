module Test.Node.Buffer.Effect (test) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer as Buffer
import Node.Buffer (BufferValueType(..))
import Node.Buffer.Effect (toArray, concat', fromArray, fill, copy, readString, fromString, toString, freeze, thaw, read, write, create, getAtOffset)
import Node.Encoding (Encoding(..))
import Test.Assert (assertEqual)

test :: Effect Unit
test = do
  log "Testing Node.Buffer.Effect ..."

  log " - create"
  testCreate

  log " - freeze"
  testFreeze

  log " - thaw"
  testThaw

  log " - Reading and writing"
  testReadWrite

  log " - fromArray"
  testFromArray

  log " - toArray"
  testToArray

  log " - fromString"
  testFromString

  log " - toString"
  testToString

  log " - readString"
  testReadString

  log " - copy"
  testCopy

  log " - fill"
  testFill

  log " - concat'"
  testConcat'

  log " - getAtOffset"
  testGetAtOffset

testCreate :: Effect Unit
testCreate = do
  buf <- create 3 >>= toArray
  assertEqual {expected: [0, 0, 0], actual: buf}

testFreeze :: Effect Unit
testFreeze = do
  buf <- fromArray [1, 2, 3] >>= freeze
  assertEqual {expected: [1, 2, 3], actual: Buffer.toArray buf}

testThaw :: Effect Unit
testThaw = do
  buf <- (thaw $ Buffer.fromArray [1, 2, 3]) >>= toArray
  assertEqual {expected: [1, 2, 3], actual: buf}

testReadWrite :: Effect Unit
testReadWrite = do
  buf <- create 1
  let val = 42
  write UInt8 val 0 buf
  readVal <- read UInt8 0 buf

  assertEqual {expected: val, actual: readVal}

testFromArray :: Effect Unit
testFromArray = do
  buf <- fromArray [1,2,3,4,5]
  readVal <- read UInt8 2 buf

  assertEqual {expected: 3, actual: readVal}

testToArray :: Effect Unit
testToArray = do
  let val = [1,2,67,3,3,7,8,3,4,237]

  buf    <- fromArray val
  valOut <- toArray buf

  assertEqual {expected: val, actual: valOut}

testFromString :: Effect Unit
testFromString = do
  let str = "hello, world"

  buf <- fromString str ASCII
  val <- read UInt8 6 buf

  assertEqual {expected: 32, actual: val} -- ASCII space

testToString :: Effect Unit
testToString = do
  let str = "hello, world"

  buf    <- fromString str ASCII
  strOut <- toString ASCII buf

  assertEqual {expected: str, actual: strOut}

testReadString :: Effect Unit
testReadString = do
  let str = "hello, world"

  buf    <- fromString str ASCII
  strOut <- readString ASCII 7 12 buf

  assertEqual {expected: "world", actual: strOut}

testCopy :: Effect Unit
testCopy = do
  buf1 <- fromArray [1,2,3,4,5]
  buf2 <- fromArray [10,9,8,7,6]

  copied <- copy 0 3 buf1 2 buf2
  out    <- toArray buf2

  assertEqual {expected: 3, actual: copied}
  assertEqual {expected: [10,9,1,2,3], actual: out}

testFill :: Effect Unit
testFill = do
  buf <- fromArray [1,1,1,1,1]
  fill 42 2 4 buf
  out <- toArray buf

  assertEqual {expected: [1,1,42,42,1], actual: out}

testConcat' :: Effect Unit
testConcat' = do
  bufs <- traverse fromArray $ map (\x -> [x, x+1, x+2]) [0,3,6,9,12]
  buf  <- concat' bufs 15
  out  <- toArray buf

  assertEqual {expected: [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14], actual: out}

testGetAtOffset :: Effect Unit
testGetAtOffset = do
  buf  <- fromArray [1, 2, 3, 4]
  o1 <- getAtOffset 1 buf
  o4 <- getAtOffset 4 buf
  om1 <- getAtOffset (-1) buf

  assertEqual {expected: Just 2, actual: o1}
  assertEqual {expected: Nothing, actual: o4}
  assertEqual {expected: Nothing, actual: om1}
