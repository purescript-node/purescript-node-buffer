-- | Types and functions for working with mutable buffers using the `ST` effect.
-- |
-- | This module can be used when mutation is a local effect.
module Node.Buffer.ST
  ( STBuffer
  , run
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

import Control.Monad.ST (ST, kind Region)
import Control.Monad.ST as ST
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Node.Buffer (Buffer, BufferValueType, Octet, Offset)
import Node.Buffer as Buffer
import Node.Encoding (Encoding, encodingToNode)
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable buffer for use with `ST`
-- |
-- | The type parameter represents the memory region which the buffer belongs to.
foreign import data STBuffer :: Region -> Type

usingFromFrozen :: forall a h. (Buffer -> a) -> STBuffer h -> ST h a
usingFromFrozen f buf = unsafeCoerce \_ -> f $ unsafeCoerce buf

usingToFrozen :: forall a h. (a -> Buffer) -> a -> ST h (STBuffer h)
usingToFrozen f x = unsafeCoerce \_ -> unsafeCoerce $ f x

-- | Runs an effect creating a mutable buffer, then freezes the buffer and returns it, without copying it.
run :: forall h. ST h (STBuffer h) -> Buffer
run st = ST.run (unsafeCoerce st)

-- | Creates a new buffer of the specified size.
create :: forall h. Int -> ST h (STBuffer h)
create = usingToFrozen Buffer.create

-- | Creates an immutable copy of a mutable buffer.
freeze :: forall h. STBuffer h -> ST h Buffer
freeze = copyImpl

-- | Creates a mutable copy of an immutable buffer.
thaw :: forall h. Buffer -> ST h (STBuffer h)
thaw = copyImpl

foreign import copyImpl :: forall h a b. a -> ST h b

-- | Creates a new buffer from an array of octets, sized to match the array.
fromArray :: forall h. Array Octet -> ST h (STBuffer h)
fromArray = usingToFrozen Buffer.fromArray

-- | Creates a new buffer from a string with the specified encoding, sized to
-- | match the string.
fromString :: forall h. String -> Encoding -> ST h (STBuffer h)
fromString s = usingToFrozen $ Buffer.fromString s

-- | Creates a buffer view from a JS ArrayByffer without copying data.
fromArrayBuffer :: forall h. ArrayBuffer -> ST h (STBuffer h)
fromArrayBuffer = usingToFrozen Buffer.fromArrayBuffer

-- | Copies the data in the buffer to a new JS ArrayBuffer
toArrayBuffer :: forall h. STBuffer h -> ST h ArrayBuffer
toArrayBuffer = usingFromFrozen Buffer.toArrayBuffer

-- | Reads a numeric value from a buffer at the specified offset.
read :: forall h. BufferValueType -> Offset -> STBuffer h -> ST h Int
read t o = usingFromFrozen $ Buffer.read t o

-- | Reads a section of a buffer as a string with the specified encoding.
readString :: forall h. Encoding -> Offset -> Offset -> STBuffer h -> ST h String
readString e o o' = usingFromFrozen $ Buffer.readString e o o'

-- | Reads the buffer as a string with the specified encoding.
toString :: forall h. Encoding -> STBuffer h -> ST h String
toString e = usingFromFrozen $ Buffer.toString e

-- | Writes a numeric value to a buffer at the specified offset.
write :: forall h. BufferValueType -> Int -> Offset -> STBuffer h -> ST h Unit
write = writeImpl <<< show

foreign import writeImpl :: forall h. String -> Int -> Offset -> STBuffer h -> ST h Unit

-- | Writes octets from a string to a buffer at the specified offset. Multi-byte
-- | characters will not be written to the buffer if there is not enough capacity
-- | to write them fully. The number of bytes written is returned.
writeString :: forall h. Encoding -> Offset -> Int -> String -> STBuffer h -> ST h Int
writeString = writeStringImpl <<< encodingToNode

foreign import writeStringImpl ::
  forall h. String -> Offset -> Int -> String -> STBuffer h -> ST h Int

-- | Creates an array of octets from a buffer's contents.
toArray :: forall h. STBuffer h -> ST h (Array Octet)
toArray = usingFromFrozen Buffer.toArray

-- | Reads an octet from a buffer at the specified offset.
getAtOffset :: forall h. Offset -> STBuffer h -> ST h (Maybe Octet)
getAtOffset o = usingFromFrozen $ Buffer.getAtOffset o

-- | Writes an octet in the buffer at the specified offset.
foreign import setAtOffset :: forall h. Offset -> Offset -> STBuffer h -> ST h Unit

-- | Returns the size of a buffer.
size :: forall h. STBuffer h -> ST h Int
size = usingFromFrozen Buffer.size

-- | Concatenates a list of buffers.
concat :: forall h. Array (STBuffer h) -> ST h (STBuffer h)
concat arrs = unsafeCoerce \_ -> Buffer.concat (unsafeCoerce arrs)

-- | Concatenates a list of buffers, combining them into a new buffer of the
-- | specified length.
concat' :: forall h. Array (STBuffer h) -> Int -> ST h (STBuffer h)
concat' arrs n = unsafeCoerce \_ -> Buffer.concat' (unsafeCoerce arrs) n

-- | Copies a section of a source buffer into a target buffer at the specified
-- | offset, and returns the number of octets copied.
foreign import copy :: forall h. Offset -> Offset -> STBuffer h -> Offset -> STBuffer h -> ST h Int

-- | Fills a range in a buffer with the specified octet.
foreign import fill :: forall h. Octet -> Offset -> Offset -> STBuffer h -> ST h Unit
