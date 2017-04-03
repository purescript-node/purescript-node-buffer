module Node.Buffer
  ( Octet()
  , Offset()
  , Buffer()
  , BUFFER()
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
import Control.Monad.Eff (Eff, kind Effect)
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

-- | Effect for buffer creation, reading, or writing.
foreign import data BUFFER :: Effect

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
foreign import create :: forall e. Int -> Eff (buffer :: BUFFER | e) Buffer

-- | Creates a new buffer from an array of octets, sized to match the array.
foreign import fromArray :: forall e. Array Octet -> Eff (buffer :: BUFFER | e) Buffer

-- | Creates a new buffer from a string with the specified encoding, sized to
-- | match the string.
fromString :: forall e. String -> Encoding -> Eff (buffer :: BUFFER | e) Buffer
fromString str = fromStringImpl str <<< encodingToNode

foreign import fromStringImpl :: forall e. String -> String -> Eff (buffer :: BUFFER | e) Buffer

-- | Reads a numeric value from a buffer at the specified offset.
read :: forall e. BufferValueType -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Int
read = readImpl <<< show

foreign import readImpl :: forall e. String -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Int

-- | Reads a section of a buffer as a string with the specified encoding.
readString :: forall e. Encoding -> Offset -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) String
readString = readStringImpl <<< encodingToNode

foreign import readStringImpl ::
  forall e. String -> Offset -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) String

-- | Reads the buffer as a string with the specified encoding.
toString :: forall e. Encoding -> Buffer -> Eff (buffer :: BUFFER | e) String
toString = toStringImpl <<< encodingToNode

foreign import toStringImpl :: forall e. String -> Buffer -> Eff (buffer :: BUFFER | e) String

-- | Writes a numeric value to a buffer at the specified offset.
write :: forall e. BufferValueType -> Int -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Unit
write = writeImpl <<< show

foreign import writeImpl ::
  forall e. String -> Int -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Unit

-- | Writes octets from a string to a buffer at the specified offset. Multi-byte
-- | characters will not be written to the buffer if there is not enough capacity
-- | to write them fully. The number of bytes written is returned.
writeString :: forall e. Encoding -> Offset -> Int -> String -> Buffer -> Eff (buffer :: BUFFER | e) Int
writeString = writeStringImpl <<< encodingToNode

foreign import writeStringImpl ::
  forall e. String -> Offset -> Int -> String -> Buffer -> Eff (buffer :: BUFFER | e) Int

-- | Creates an array of octets from a buffer's contents.
foreign import toArray :: forall e. Buffer -> Eff (buffer :: BUFFER | e) (Array Octet)

-- | Reads an octet from a buffer at the specified offset.
getAtOffset :: forall e. Offset -> Buffer -> Eff (buffer :: BUFFER | e) (Maybe Octet)
getAtOffset = getAtOffsetImpl Just Nothing

foreign import getAtOffsetImpl ::
  forall e. (Octet -> Maybe Octet) -> Maybe Octet -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) (Maybe Octet)

-- | Writes an octet in the buffer at the specified offset.
foreign import setAtOffset ::
  forall e. Octet -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Unit

-- | Returns the size of a buffer.
foreign import size :: forall e. Buffer -> Eff (buffer :: BUFFER | e) Int

-- | Concatenates a list of buffers.
foreign import concat :: forall e. Array Buffer -> Eff (buffer :: BUFFER | e) Buffer

-- | Concatenates a list of buffers, combining them into a new buffer of the
-- | specified length.
foreign import concat' :: forall e. Array Buffer -> Int -> Eff (buffer :: BUFFER | e) Buffer

-- | Copies a section of a source buffer into a target buffer at the specified
-- | offset, and returns the number of octets copied.
foreign import copy :: forall e. Offset -> Offset -> Buffer -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Int

-- | Fills a range in a buffer with the specified octet.
foreign import fill ::
  forall e. Octet -> Offset -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Unit
