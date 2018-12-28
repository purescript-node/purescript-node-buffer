module Node.Buffer.Mutable
 ( class MutableBuffer
 , create
 , freeze
 , thaw
 , fromArray
 , fromString
 , fromArrayBuffer
 , toArrayBuffer
 , read
 , readString
 , toString
 , write
 , writeString
 , toArray
 , getAtOffset
 , setAtOffset
 , size
 , concat
 , concat'
 , copy
 , fill
 , EffectBuffer
 , STBuffer
 , runST
 ) where

import Prelude

import Control.Monad.ST (ST, kind Region)
import Control.Monad.ST as ST
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Effect (Effect)
import Node.Buffer (Buffer, BufferValueType, Octet, Offset)
import Node.Buffer as Buffer
import Node.Encoding (Encoding, encodingToNode)
import Unsafe.Coerce (unsafeCoerce)

-- | A type class for mutable buffers `b` where operations on those buffers are
-- | represented by a particular effect type `e`.
class MutableBuffer b e | e -> b, b -> e where

  -- | Creates a new buffer of the specified size.
  create :: Int -> e b

  -- | Creates an immutable copy of a mutable buffer.
  freeze :: b -> e Buffer

  -- | Creates a mutable copy of an immutable buffer.
  thaw :: Buffer -> e b

  -- | Creates a new buffer from an array of octets, sized to match the array.
  fromArray :: Array Octet -> e b

  -- | Creates a new buffer from a string with the specified encoding, sized to
  -- | match the string.
  fromString :: String -> Encoding -> e b

  -- | Creates a buffer view from a JS ArrayByffer without copying data.
  fromArrayBuffer :: ArrayBuffer -> e b

  -- | Copies the data in the buffer to a new JS ArrayBuffer
  toArrayBuffer :: b -> e ArrayBuffer

  -- | Reads a numeric value from a buffer at the specified offset.
  read :: BufferValueType -> Offset -> b -> e Int

  -- | Reads a section of a buffer as a string with the specified encoding.
  readString :: Encoding -> Offset -> Offset -> b -> e String

  -- | Reads the buffer as a string with the specified encoding.
  toString :: Encoding -> b -> e String

  -- | Writes a numeric value to a buffer at the specified offset.
  write :: BufferValueType -> Int -> Offset -> b -> e Unit

  -- | Writes octets from a string to a buffer at the specified offset. Multi-byte
  -- | characters will not be written to the buffer if there is not enough capacity
  -- | to write them fully. The number of bytes written is returned.
  writeString :: Encoding -> Offset -> Int -> String -> b -> e Int

  -- | Creates an array of octets from a buffer's contents.
  toArray :: b -> e (Array Octet)

  -- | Reads an octet from a buffer at the specified offset.
  getAtOffset :: Offset -> b -> e (Maybe Octet)

  -- | Writes an octet in the buffer at the specified offset.
  setAtOffset :: Octet -> Offset -> b -> e Unit

  -- | Returns the size of a buffer.
  size :: b -> e Int

  -- | Concatenates a list of buffers.
  concat :: Array b -> e b

  -- | Concatenates a list of buffers, combining them into a new buffer of the
  -- | specified length.
  concat' :: Array b -> Int -> e b

  -- | Copies a section of a source buffer into a target buffer at the specified
  -- | offset, and returns the number of octets copied.
  copy :: Offset -> Offset -> b -> Offset -> b -> e Int

  -- | Fills a range in a buffer with the specified octet.
  fill :: Octet -> Offset -> Offset -> b -> e Unit

-- | A reference to a mutable buffer for use with `Effect`
foreign import data EffectBuffer :: Type

-- | A reference to a mutable buffer for use with `ST`
-- |
-- | The type parameter represents the memory region which the buffer belongs to.
foreign import data STBuffer :: Region -> Type

-- | Runs an effect creating an `STBuffer` then freezes the buffer and returns
-- | it, without unneccessary copying.
runST :: forall h. ST h (STBuffer h) -> Buffer
runST st = ST.run (unsafeCoerce st)

instance mutableBufferEffect :: MutableBuffer EffectBuffer Effect where
  create = createImpl
  freeze = copyAllImpl
  thaw = copyAllImpl
  fromArray = fromArrayImpl
  fromString = fromStringImpl
  fromArrayBuffer = fromArrayBufferImpl
  toArrayBuffer = toArrayBufferImpl
  read = readImpl
  readString = readStringImpl
  toString = toStringImpl
  write = writeImpl
  writeString = writeStringImpl
  toArray = toArrayImpl
  getAtOffset = getAtOffsetImpl
  setAtOffset = setAtOffsetImpl
  size = sizeImpl
  concat = concatImpl
  concat' = concatImpl'
  copy = copyImpl
  fill = fillImpl

instance mutableBufferST :: MutableBuffer (STBuffer h) (ST h) where
  create = createImpl
  freeze = copyAllImpl
  thaw = copyAllImpl
  fromArray = fromArrayImpl
  fromString = fromStringImpl
  fromArrayBuffer = fromArrayBufferImpl
  toArrayBuffer = toArrayBufferImpl
  read = readImpl
  readString = readStringImpl
  toString = toStringImpl
  write = writeImpl
  writeString = writeStringImpl
  toArray = toArrayImpl
  getAtOffset = getAtOffsetImpl
  setAtOffset = setAtOffsetImpl
  size = sizeImpl
  concat = concatImpl
  concat' = concatImpl'
  copy = copyImpl
  fill = fillImpl

usingFromFrozen :: forall b e a. (Buffer -> a) -> b -> e a
usingFromFrozen f buf = unsafeCoerce \_ -> f $ unsafeCoerce buf

usingToFrozen :: forall b e a. (a -> Buffer) -> a -> e b
usingToFrozen f x = unsafeCoerce \_ -> unsafeCoerce $ f x

createImpl :: forall b e. Int -> e b
createImpl = usingToFrozen Buffer.create

foreign import copyAllImpl :: forall a b e. a -> e b

fromArrayImpl :: forall b e. Array Octet -> e b
fromArrayImpl = usingToFrozen Buffer.fromArray

fromStringImpl :: forall b e. String -> Encoding -> e b
fromStringImpl s = usingToFrozen $ Buffer.fromString s

fromArrayBufferImpl :: forall b e. ArrayBuffer -> e b
fromArrayBufferImpl = usingToFrozen Buffer.fromArrayBuffer

toArrayBufferImpl :: forall b e. b -> e ArrayBuffer
toArrayBufferImpl = usingFromFrozen Buffer.toArrayBuffer

readImpl :: forall b e. BufferValueType -> Offset -> b -> e Int
readImpl t o = usingFromFrozen $ Buffer.read t o

readStringImpl :: forall b e. Encoding -> Offset -> Offset -> b -> e String
readStringImpl e o o' = usingFromFrozen $ Buffer.readString e o o'

toStringImpl :: forall b e. Encoding -> b -> e String
toStringImpl e = usingFromFrozen $ Buffer.toString e

writeImpl :: forall b e. BufferValueType -> Int -> Offset -> b -> e Unit
writeImpl = writeInternal <<< show

foreign import writeInternal :: forall b e. String -> Int -> Offset -> b -> e Unit

writeStringImpl :: forall b e. Encoding -> Offset -> Int -> String -> b -> e Int
writeStringImpl = writeStringInternal <<< encodingToNode

foreign import writeStringInternal ::
  forall b e. String -> Offset -> Int -> String -> b -> e Int

toArrayImpl :: forall b e. b -> e (Array Octet)
toArrayImpl = usingFromFrozen Buffer.toArray

getAtOffsetImpl :: forall b e. Offset -> b -> e (Maybe Octet)
getAtOffsetImpl o = usingFromFrozen $ Buffer.getAtOffset o

foreign import setAtOffsetImpl :: forall b e. Octet -> Offset -> b -> e Unit

sizeImpl :: forall b e. b -> e Int
sizeImpl = usingFromFrozen Buffer.size

concatImpl :: forall b e. Array b -> e b
concatImpl arrs = unsafeCoerce \_ -> Buffer.concat (unsafeCoerce arrs)

concatImpl' :: forall b e. Array b -> Int -> e b
concatImpl' arrs n = unsafeCoerce \_ -> Buffer.concat' (unsafeCoerce arrs) n

foreign import copyImpl :: forall b e. Offset -> Offset -> b -> Offset -> b -> e Int

foreign import fillImpl :: forall b e. Octet -> Offset -> Offset -> b -> e Unit
