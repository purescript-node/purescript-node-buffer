module Node.Buffer
  ( Octet()
  , Offset()
  , Buffer()
  , BufferWrite()
  , BufferValueType(..)
  , create
  , fromArray
  , fromString
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
import Control.Monad.Eff
import Data.Maybe
import Node.Encoding

-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 255.
type Octet = Int

-- | Type synonym indicating the value refers to an offset in a buffer.
type Offset = Int

-- | An instance of Node's Buffer class.
foreign import data Buffer :: *

instance showBuffer :: Show Buffer where
  show = showImpl

foreign import showImpl :: Buffer -> String

-- | Effect for buffer modification.
foreign import data BufferWrite :: !

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
foreign import create :: Int -> Buffer

-- | Creates a new buffer from an array of octets, sized to match the array.
foreign import fromArray :: Array Octet -> Buffer

-- | Creates a new buffer from a string with the specified encoding, sized to
-- | match the string.
fromString :: String -> Encoding -> Buffer
fromString str = fromStringImpl str <<< show

foreign import fromStringImpl :: String -> String -> Buffer

-- | Reads a numeric value from a buffer at the specified offset.
read :: BufferValueType -> Offset -> Buffer -> Int
read = readImpl <<< show

foreign import readImpl :: String -> Offset -> Buffer -> Int

-- | Reads a section of a buffer as a string with the specified encoding.
readString :: Encoding -> Offset -> Offset -> Buffer -> String
readString = readStringImpl <<< show

foreign import readStringImpl :: String -> Offset -> Offset -> Buffer -> String

-- | Reads the buffer as a string with the specified encoding.
toString :: Encoding -> Buffer -> String
toString = toStringImpl <<< show

foreign import toStringImpl :: String -> Buffer -> String

-- | Writes a numeric value to a buffer at the specified offset.
write :: forall e. BufferValueType -> Int -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
write = writeImpl <<< show

foreign import writeImpl ::
  forall e. String -> Int -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit

-- | Writes octets from a string to a buffer at the specified offset. Multi-byte
-- | characters will not be written to the buffer if there is not enough capacity
-- | to write them fully. The number of bytes written is returned.
writeString :: forall e. Encoding -> Offset -> Int -> String -> Buffer -> Eff (buffer :: BufferWrite | e) Int
writeString = writeStringImpl <<< show

foreign import writeStringImpl ::
  forall e. String -> Offset -> Int -> String -> Buffer -> Eff (buffer :: BufferWrite | e) Int

-- | Creates an array of octets from a buffer's contents.
foreign import toArray :: Buffer -> Array Octet

-- | Reads an octet from a buffer at the specified offset.
getAtOffset :: Offset -> Buffer -> Maybe Octet
getAtOffset = getAtOffsetImpl Just Nothing

foreign import getAtOffsetImpl ::
  (Octet -> Maybe Octet) -> Maybe Octet -> Offset -> Buffer -> Maybe Octet

-- | Writes an octet in the buffer at the specified offset.
foreign import setAtOffset ::
  forall e. Octet -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit

-- | Returns the size of a buffer.
foreign import size :: Buffer -> Int

-- | Concatenates a list of buffers.
foreign import concat :: Array Buffer -> Buffer

-- | Concatenates a list of buffers, combining them into a new buffer of the
-- | specified length.
foreign import concat' :: Array Buffer -> Int -> Buffer

-- | Copies a section of a source buffer into a target buffer at the specified
-- | offset.
foreign import copy :: Offset -> Offset -> Buffer -> Offset -> Buffer -> Buffer

-- | Fills a range in a buffer with the specified octet.
foreign import fill ::
  forall e. Octet -> Offset -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
