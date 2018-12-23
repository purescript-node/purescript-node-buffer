-- | Types and functions for working with mutable buffers using `Effect`.
module Node.Buffer.Effect
  ( EffectBuffer
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
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Effect (Effect)
import Node.Buffer (Buffer, BufferValueType, Octet, Offset)
import Node.Buffer as Buffer
import Node.Encoding (Encoding, encodingToNode)
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable buffer, for use with `Effect`
foreign import data EffectBuffer :: Type

usingFromFrozen :: forall a. (Buffer -> a) -> EffectBuffer -> Effect a
usingFromFrozen f buf = unsafeCoerce \_ -> f $ unsafeCoerce buf

usingToFrozen :: forall a. (a -> Buffer) -> a -> Effect EffectBuffer
usingToFrozen f x = unsafeCoerce \_ -> unsafeCoerce $ f x

-- | Creates a new buffer of the specified size.
create :: Int -> Effect EffectBuffer
create = usingToFrozen Buffer.create

-- | Creates an immutable copy of a mutable buffer.
freeze :: EffectBuffer -> Effect Buffer
freeze = copyImpl

-- | Creates a mutable copy of an immutable buffer.
thaw :: Buffer -> Effect EffectBuffer
thaw = copyImpl

foreign import copyImpl :: forall a b. a -> Effect b

-- | Creates a new buffer from an array of octets, sized to match the array.
fromArray :: Array Octet -> Effect EffectBuffer
fromArray = usingToFrozen Buffer.fromArray

-- | Creates a new buffer from a string with the specified encoding, sized to
-- | match the string.
fromString :: String -> Encoding -> Effect EffectBuffer
fromString s = usingToFrozen $ Buffer.fromString s

-- | Creates a buffer view from a JS ArrayByffer without copying data.
fromArrayBuffer :: ArrayBuffer -> Effect EffectBuffer
fromArrayBuffer = usingToFrozen Buffer.fromArrayBuffer

-- | Copies the data in the buffer to a new JS ArrayBuffer
toArrayBuffer :: EffectBuffer -> Effect ArrayBuffer
toArrayBuffer = usingFromFrozen Buffer.toArrayBuffer

-- | Reads a numeric value from a buffer at the specified offset.
read :: BufferValueType -> Offset -> EffectBuffer -> Effect Int
read t o = usingFromFrozen $ Buffer.read t o

-- | Reads a section of a buffer as a string with the specified encoding.
readString :: Encoding -> Offset -> Offset -> EffectBuffer -> Effect String
readString e o o' = usingFromFrozen $ Buffer.readString e o o'

-- | Reads the buffer as a string with the specified encoding.
toString :: Encoding -> EffectBuffer -> Effect String
toString e = usingFromFrozen $ Buffer.toString e

-- | Writes a numeric value to a buffer at the specified offset.
write :: BufferValueType -> Int -> Offset -> EffectBuffer -> Effect Unit
write = writeImpl <<< show

foreign import writeImpl :: String -> Int -> Offset -> EffectBuffer -> Effect Unit

-- | Writes octets from a string to a buffer at the specified offset. Multi-byte
-- | characters will not be written to the buffer if there is not enough capacity
-- | to write them fully. The number of bytes written is returned.
writeString :: Encoding -> Offset -> Int -> String -> EffectBuffer -> Effect Int
writeString = writeStringImpl <<< encodingToNode

foreign import writeStringImpl ::
  String -> Offset -> Int -> String -> EffectBuffer -> Effect Int

-- | Creates an array of octets from a buffer's contents.
toArray :: EffectBuffer -> Effect (Array Octet)
toArray = usingFromFrozen Buffer.toArray

-- | Reads an octet from a buffer at the specified offset.
getAtOffset :: Offset -> EffectBuffer -> Effect (Maybe Octet)
getAtOffset o = usingFromFrozen $ Buffer.getAtOffset o

-- | Writes an octet in the buffer at the specified offset.
foreign import setAtOffset :: Octet -> Offset -> EffectBuffer -> Effect Unit

-- | Returns the size of a buffer.
size :: EffectBuffer -> Effect Int
size = usingFromFrozen Buffer.size

-- | Concatenates a list of buffers.
concat :: Array EffectBuffer -> Effect EffectBuffer
concat arrs = unsafeCoerce \_ -> Buffer.concat (unsafeCoerce arrs)

-- | Concatenates a list of buffers, combining them into a new buffer of the
-- | specified length.
concat' :: Array EffectBuffer -> Int -> Effect EffectBuffer
concat' arrs n = unsafeCoerce \_ -> Buffer.concat' (unsafeCoerce arrs) n

-- | Copies a section of a source buffer into a target buffer at the specified
-- | offset, and returns the number of octets copied.
foreign import copy :: Offset -> Offset -> EffectBuffer -> Offset -> EffectBuffer -> Effect Int

-- | Fills a range in a buffer with the specified octet.
foreign import fill :: Octet -> Offset -> Offset -> EffectBuffer -> Effect Unit
