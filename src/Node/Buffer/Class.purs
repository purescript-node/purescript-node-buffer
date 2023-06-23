module Node.Buffer.Class
  ( class MutableBuffer
  , create
  , freeze
  , unsafeFreeze
  , thaw
  , unsafeThaw
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
  , slice
  , size
  , concat
  , concat'
  , copy
  , fill
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Effect (Effect)
import Node.Buffer as Buffer
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.ST as ST
import Node.Buffer.Types (BufferValueType, Octet, Offset)
import Node.Encoding (Encoding)

-- | A type class for mutable buffers `buf` where operations on those buffers are
-- | represented by a particular monadic effect type `m`.
class Monad m <= MutableBuffer buf m | buf -> m where

  -- | Creates a new buffer of the specified size.
  create :: Int -> m buf

  -- | Creates an immutable copy of a mutable buffer.
  freeze :: buf -> m ImmutableBuffer

  -- | O(1). Convert a mutable buffer to an immutable buffer, without copying. The
  -- | mutable buffer must not be mutated afterwards.
  unsafeFreeze :: buf -> m ImmutableBuffer

  -- | Creates a mutable copy of an immutable buffer.
  thaw :: ImmutableBuffer -> m buf

  -- | O(1) Convert an immutable buffer to a mutable buffer, without copying. The
  -- | input buffer must not be used afterward.
  unsafeThaw :: ImmutableBuffer -> m buf

  -- | Creates a new buffer from an array of octets, sized to match the array.
  fromArray :: Array Octet -> m buf

  -- | Creates a new buffer from a string with the specified encoding, sized to
  -- | match the string.
  fromString :: String -> Encoding -> m buf

  -- | Creates a buffer view from a JS ArrayByffer without copying data.
  fromArrayBuffer :: ArrayBuffer -> m buf

  -- | Copies the data in the buffer to a new JS ArrayBuffer
  toArrayBuffer :: buf -> m ArrayBuffer

  -- | Reads a numeric value from a buffer at the specified offset.
  read :: BufferValueType -> Offset -> buf -> m Number

  -- | Reads a section of a buffer as a string with the specified encoding.
  readString :: Encoding -> Offset -> Offset -> buf -> m String

  -- | Reads the buffer as a string with the specified encoding.
  toString :: Encoding -> buf -> m String

  -- | Writes a numeric value to a buffer at the specified offset.
  write :: BufferValueType -> Number -> Offset -> buf -> m Unit

  -- | Writes octets from a string to a buffer at the specified offset. Multi-byte
  -- | characters will not be written to the buffer if there is not enough capacity
  -- | to write them fully. The number of bytes written is returned.
  writeString :: Encoding -> Offset -> Int -> String -> buf -> m Int

  -- | Creates an array of octets from a buffer's contents.
  toArray :: buf -> m (Array Octet)

  -- | Reads an octet from a buffer at the specified offset.
  getAtOffset :: Offset -> buf -> m (Maybe Octet)

  -- | Writes an octet in the buffer at the specified offset.
  setAtOffset :: Octet -> Offset -> buf -> m Unit

  -- | Creates a new buffer slice that acts like a window on the original buffer.
  -- | Writing to the slice buffer updates the original buffer and vice-versa.
  slice :: Offset -> Offset -> buf -> buf

  -- | Returns the size of a buffer.
  size :: buf -> m Int

  -- | Concatenates a list of buffers.
  concat :: Array buf -> m buf

  -- | Concatenates a list of buffers, combining them into a new buffer of the
  -- | specified length.
  concat' :: Array buf -> Int -> m buf

  -- | Copies a section of a source buffer into a target buffer at the specified
  -- | offset, and returns the number of octets copied.
  copy :: Offset -> Offset -> buf -> Offset -> buf -> m Int

  -- | Fills a range in a buffer with the specified octet.
  fill :: Octet -> Offset -> Offset -> buf -> m Unit

instance mutableBufferEffect :: MutableBuffer Buffer.Buffer Effect where
  create = Buffer.create
  freeze = Buffer.freeze
  unsafeFreeze = Buffer.unsafeFreeze
  thaw = Buffer.thaw
  unsafeThaw = Buffer.unsafeThaw
  fromArray = Buffer.fromArray
  fromString = Buffer.fromString
  fromArrayBuffer = Buffer.fromArrayBuffer
  toArrayBuffer = Buffer.toArrayBuffer
  read = Buffer.read
  readString = Buffer.readString
  toString = Buffer.toString
  write = Buffer.write
  writeString = Buffer.writeString
  toArray = Buffer.toArray
  getAtOffset = Buffer.getAtOffset
  setAtOffset = Buffer.setAtOffset
  slice = Buffer.slice
  size = Buffer.size
  concat = Buffer.concat
  concat' = Buffer.concat'
  copy = Buffer.copy
  fill = Buffer.fill

instance mutableBufferST :: MutableBuffer (ST.STBuffer h) (ST h) where
  create = ST.create
  freeze = ST.freeze
  unsafeFreeze = ST.unsafeFreeze
  thaw = ST.thaw
  unsafeThaw = ST.unsafeThaw
  fromArray = ST.fromArray
  fromString = ST.fromString
  fromArrayBuffer = ST.fromArrayBuffer
  toArrayBuffer = ST.toArrayBuffer
  read = ST.read
  readString = ST.readString
  toString = ST.toString
  write = ST.write
  writeString = ST.writeString
  toArray = ST.toArray
  getAtOffset = ST.getAtOffset
  setAtOffset = ST.setAtOffset
  slice = ST.slice
  size = ST.size
  concat = ST.concat
  concat' = ST.concat'
  copy = ST.copy
  fill = ST.fill
