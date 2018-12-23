module Test.Node.Buffer.ST (test) where

import Prelude

import Control.Monad.ST as ST
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer as Buffer
import Node.Buffer (BufferValueType(..))
import Node.Buffer.ST (freeze, thaw, concat', copy, create, fill, fromArray, fromString, getAtOffset, read, readString, run, toArray, toString, write)
import Node.Encoding (Encoding(..))
import Test.Assert (assertEqual)

test :: Effect Unit
test = do
  log "Testing Node.Buffer.ST ..."

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
  let buf = run (create 3)
  assertEqual {expected: [0, 0, 0], actual: Buffer.toArray buf}

testFreeze :: Effect Unit
testFreeze = do
  let buf = ST.run (fromArray [1, 2, 3] >>= freeze)
  assertEqual {expected: [1, 2, 3], actual: Buffer.toArray buf}

testThaw :: Effect Unit
testThaw = do
  let buf = run (thaw $ Buffer.fromArray [1, 2, 3])
  assertEqual {expected: [1, 2, 3], actual: Buffer.toArray buf}

testReadWrite :: Effect Unit
testReadWrite = do
  let val = 42
      readVal = ST.run do
        buf <- create 1
        write UInt8 val 0 buf
        read UInt8 0 buf

  assertEqual {expected: val, actual: readVal}

testFromArray :: Effect Unit
testFromArray = do
  let readVal = ST.run do
        buf <- fromArray [1,2,3,4,5]
        read UInt8 2 buf

  assertEqual {expected: 3, actual: readVal}

testToArray :: Effect Unit
testToArray = do
  let val = [1,2,67,3,3,7,8,3,4,237]
      valOut = ST.run do
        buf <- fromArray val
        toArray buf

  assertEqual {expected: val, actual: valOut}

testFromString :: Effect Unit
testFromString = do
  let str = "hello, world"
      val = ST.run do
        buf <- fromString str ASCII
        read UInt8 6 buf

  assertEqual {expected: 32, actual: val} -- ASCII space

testToString :: Effect Unit
testToString = do
  let str = "hello, world"
      strOut = ST.run do
        buf <- fromString str ASCII
        toString ASCII buf

  assertEqual {expected: str, actual: strOut}

testReadString :: Effect Unit
testReadString = do
  let str = "hello, world"
      strOut = ST.run do
        buf <- fromString str ASCII
        readString ASCII 7 12 buf

  assertEqual {expected: "world", actual: strOut}

testCopy :: Effect Unit
testCopy = do
  let {copied, out} = ST.run do
        buf1 <- fromArray [1,2,3,4,5]
        buf2 <- fromArray [10,9,8,7,6]
        copied <- copy 0 3 buf1 2 buf2
        out <- toArray buf2
        pure {copied, out}

  assertEqual {expected: 3, actual: copied}
  assertEqual {expected: [10,9,1,2,3], actual: out}

testFill :: Effect Unit
testFill = do
  let out = ST.run do
        buf <- fromArray [1,1,1,1,1]
        fill 42 2 4 buf
        toArray buf

  assertEqual {expected: [1,1,42,42,1], actual: out}

testConcat' :: Effect Unit
testConcat' = do
  let out = ST.run do
        bufs <- traverse fromArray $ map (\x -> [x, x+1, x+2]) [0,3,6,9,12]
        buf  <- concat' bufs 15
        toArray buf

  assertEqual {expected: [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14], actual: out}

testGetAtOffset :: Effect Unit
testGetAtOffset = do
  let {o1, o4, om1} = ST.run do
        buf  <- fromArray [1, 2, 3, 4]
        o1 <- getAtOffset 1 buf
        o4 <- getAtOffset 4 buf
        om1 <- getAtOffset (-1) buf
        pure {o1, o4, om1}

  assertEqual {expected: Just 2, actual: o1}
  assertEqual {expected: Nothing, actual: o4}
  assertEqual {expected: Nothing, actual: om1}
