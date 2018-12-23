module Test.Node.Buffer (test) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Buffer (Buffer, BufferValueType(..))
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Test.Assert (assertEqual, assertTrue)

test :: Effect Unit
test = do
  log "Testing Node.Buffer ..."

  log " - show"
  testShow

  log " - eq"
  testEq

  log " - compare"
  testCompare

  log " - create"
  testCreate

  log " - fromString"
  testFromString

  log " - toString"
  testToString

  log " - toArray"
  testToArray

  log " - readString"
  testReadString

  log " - getAtOffset"
  testGetAtOffset

  log " - (to/from)ArrayBuffer"
  testToFromArrayBuffer

  log " - concat'"
  testConcat'

  log " - slice"
  testSlice

  log " - size"
  testSize

buffer123 :: Buffer
buffer123 = Buffer.fromArray [1, 2, 3]

testShow :: Effect Unit
testShow = do
  assertEqual {expected: "<Buffer 01 02 03>", actual: show buffer123}

testEq :: Effect Unit
testEq = do
  assertTrue $ buffer123 == buffer123
  assertTrue $ buffer123 == Buffer.fromArray [1, 2, 3]
  assertTrue $ buffer123 /= Buffer.fromArray [1, 2, 4]
  assertTrue $ buffer123 /= Buffer.fromArray [1, 2]

testCompare :: Effect Unit
testCompare = do
  assertEqual {expected: EQ, actual: compare buffer123 buffer123}
  assertEqual {expected: LT, actual: compare buffer123 $ Buffer.fromArray [3, 2, 1]}
  assertEqual {expected: GT, actual: compare buffer123 $ Buffer.fromArray [0, 1, 2]}

testCreate :: Effect Unit
testCreate = do
  assertEqual {expected: Buffer.fromArray [], actual: Buffer.create 0}
  assertEqual {expected: Buffer.fromArray [0, 0, 0], actual: Buffer.create 3}

testFromString :: Effect Unit
testFromString = do
  let buf = Buffer.fromString "hello, world" ASCII
  assertEqual {expected: 32, actual: Buffer.read UInt8 6 buf}

testToString :: Effect Unit
testToString = do
  let str = "hello, world"
      str' = Buffer.toString ASCII $ Buffer.fromString str ASCII
  assertEqual {expected: str, actual: str'}

testToArray :: Effect Unit
testToArray = do
  assertEqual {expected: [1, 2, 3], actual: Buffer.toArray buffer123}

testReadString :: Effect Unit
testReadString = do
  let str = "hello, world"
      str' = Buffer.readString ASCII 7 12 $ Buffer.fromString str ASCII
  assertEqual {expected: "world", actual: str'}

testGetAtOffset :: Effect Unit
testGetAtOffset = do
  assertEqual {expected: Just 2, actual: Buffer.getAtOffset 1 buffer123}
  assertEqual {expected: Nothing, actual: Buffer.getAtOffset 99 buffer123}
  assertEqual {expected: Nothing, actual: Buffer.getAtOffset (-1) buffer123}

testToFromArrayBuffer :: Effect Unit
testToFromArrayBuffer = do
  assertEqual {expected: buffer123, actual: Buffer.fromArrayBuffer $ Buffer.toArrayBuffer buffer123}

testConcat' :: Effect Unit
testConcat' = do
  let bufs = map (\x -> Buffer.fromArray [x, x+1, x+2]) [0,3,6,9,12]
      buf = Buffer.concat' bufs 15
      out = Buffer.toArray buf

  assertEqual {expected: [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14], actual: out}

testSlice :: Effect Unit
testSlice = do
  assertEqual {expected: buffer123, actual: Buffer.slice 0 3 buffer123}
  assertEqual {expected: buffer123, actual: Buffer.slice 0 4 buffer123}
  assertEqual {expected: Buffer.fromArray [2], actual: Buffer.slice 1 2 buffer123}

testSize :: Effect Unit
testSize = do
  assertEqual {expected: 0, actual: Buffer.size $ Buffer.fromArray []}
  assertEqual {expected: 3, actual: Buffer.size buffer123}
