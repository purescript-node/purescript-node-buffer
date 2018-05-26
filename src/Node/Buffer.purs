module Node.Buffer
  ( Octet()
  , Offset()
  , Buffer()
  , BufferValueType(..)
  , create
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

import Effect (Effect)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding, encodingToNode)

-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 256.
type Octet = Int

-- | Type synonym indicating the value refers to an offset in a buffer.
type Offset = Int

-- | An instance of Node's Buffer class.
foreign import data Buffer :: Type

instance showBuffer :: Show Buffer where
  show = showImpl

foreign import showImpl :: Buffer -> String

-- | Enumeration of the numeric types that can be written to a buffer.
data BufferValueType
  = UInt8
  | UInt16LE
  | UInt16BE
  | UInt32LE
  | UInt32BE
  | Int8
  | Int16LE
  | Int16BE
  | Int32LE
  | Int32BE
  | FloatLE
  | FloatBE
  | DoubleLE
  | DoubleBE

instance showBufferValueType :: Show BufferValueType where
  show UInt8    = "UInt8"
  show UInt16LE = "UInt16LE"
  show UInt16BE = "UInt16BE"
  show UInt32LE = "UInt32LE"
  show UInt32BE = "UInt32BE"
  show Int8     = "Int8"
  show Int16LE  = "Int16LE"
  show Int16BE  = "Int16BE"
  show Int32LE  = "Int32LE"
  show Int32BE  = "Int32BE"
  show FloatLE  = "FloatLE"
  show FloatBE  = "FloatBE"
  show DoubleLE = "DoubleLE"
  show DoubleBE = "DoubleBE"

-- | Creates a new buffer of the specified size.
foreign import create :: Int -> Effect Buffer

-- | Creates a new buffer from an array of octets, sized to match the array.
foreign import fromArray :: Array Octet -> Effect Buffer

-- | Creates a buffer view from a JS ArrayByffer without copying data.
--
-- Requires Node >= v5.10.0
foreign import fromArrayBuffer :: ArrayBuffer -> Effect Buffer

-- | Creates a new buffer from a string with the specified encoding, sized to
-- | match the string.
fromString :: String -> Encoding -> Effect Buffer
fromString str = fromStringImpl str <<< encodingToNode

foreign import fromStringImpl :: String -> String -> Effect Buffer

foreign import toArrayBuffer :: Buffer -> Effect ArrayBuffer

-- | Reads a numeric value from a buffer at the specified offset.
read :: BufferValueType -> Offset -> Buffer -> Effect Int
read = readImpl <<< show

foreign import readImpl :: String -> Offset -> Buffer -> Effect Int

-- | Reads a section of a buffer as a string with the specified encoding.
readString :: Encoding -> Offset -> Offset -> Buffer -> Effect String
readString = readStringImpl <<< encodingToNode

foreign import readStringImpl ::
  String -> Offset -> Offset -> Buffer -> Effect String

-- | Reads the buffer as a string with the specified encoding.
toString :: Encoding -> Buffer -> Effect String
toString = toStringImpl <<< encodingToNode

foreign import toStringImpl :: String -> Buffer -> Effect String

-- | Writes a numeric value to a buffer at the specified offset.
write :: BufferValueType -> Int -> Offset -> Buffer -> Effect Unit
write = writeImpl <<< show

foreign import writeImpl :: String -> Int -> Offset -> Buffer -> Effect Unit

-- | Writes octets from a string to a buffer at the specified offset. Multi-byte
-- | characters will not be written to the buffer if there is not enough capacity
-- | to write them fully. The number of bytes written is returned.
writeString :: Encoding -> Offset -> Int -> String -> Buffer -> Effect Int
writeString = writeStringImpl <<< encodingToNode

foreign import writeStringImpl ::
  String -> Offset -> Int -> String -> Buffer -> Effect Int

-- | Creates an array of octets from a buffer's contents.
foreign import toArray :: Buffer -> Effect (Array Octet)

-- | Reads an octet from a buffer at the specified offset.
getAtOffset :: Offset -> Buffer -> Effect (Maybe Octet)
getAtOffset = getAtOffsetImpl Just Nothing

foreign import getAtOffsetImpl ::
  (Octet -> Maybe Octet) -> Maybe Octet -> Offset -> Buffer -> Effect (Maybe Octet)

-- | Writes an octet in the buffer at the specified offset.
foreign import setAtOffset :: Octet -> Offset -> Buffer -> Effect Unit

-- | Returns the size of a buffer.
foreign import size :: Buffer -> Effect Int

-- | Concatenates a list of buffers.
foreign import concat :: Array Buffer -> Effect Buffer

-- | Concatenates a list of buffers, combining them into a new buffer of the
-- | specified length.
foreign import concat' :: Array Buffer -> Int -> Effect Buffer

-- | Copies a section of a source buffer into a target buffer at the specified
-- | offset, and returns the number of octets copied.
foreign import copy :: Offset -> Offset -> Buffer -> Offset -> Buffer -> Effect Int

-- | Fills a range in a buffer with the specified octet.
foreign import fill ::
  Octet -> Offset -> Offset -> Buffer -> Effect Unit
