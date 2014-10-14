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

import Control.Monad.Eff
import Data.Maybe
import Node.Encoding

-- |
-- Type synonym indicating the value should be an octet (0-255). If the value 
-- provided is outside this range it will be used as modulo 255.
--
type Octet = Number

-- |
-- Type synonym indicating the value refers to an offset in a buffer.
--
type Offset = Number

-- |
-- An instance of Node's Buffer class.
-- 
foreign import data Buffer :: *

instance showBuffer :: Show Buffer where
  show = showImpl
  
foreign import showImpl "var showImpl = require('util').inspect;" :: Buffer -> String

-- |
-- Effect for buffer modification.
--
foreign import data BufferWrite :: !

-- |
-- Enumeration of the numeric types that can be written to a buffer.
--
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

-- |
-- Creates a new buffer of the specified size.
--
foreign import create
  "function create (size) { \
  \  return new Buffer(size); \
  \}" :: Number -> Buffer

-- |
-- Creates a new buffer from an array of octets, sized to match the array.
--
foreign import fromArray
  "function fromArray (octets) { \
  \  return new Buffer(octets); \
  \}" :: [Octet] -> Buffer

-- |
-- Creates a new buffer from a string with the specified encoding, sized to 
-- match the string.
--
fromString :: String -> Encoding -> Buffer
fromString str = fromStringImpl str <<< show

foreign import fromStringImpl
  "function fromStringImpl (str) { \
  \  return function (encoding) { \
  \    return new Buffer(str, encoding); \
  \  }; \
  \}" :: String -> String -> Buffer

-- |
-- Reads a numeric value from a buffer at the specified offset.
--
read :: BufferValueType -> Offset -> Buffer -> Number
read = readImpl <<< show

foreign import readImpl
  "function readImpl (ty) { \
  \  return function (offset) { \
  \    return function (buf) { \
  \      return buf['read' + ty](offset); \
  \    }; \
  \  }; \
  \}" :: String -> Offset -> Buffer -> Number
  

-- |
-- Reads a section of a buffer as a string with the specified encoding.
--
readString :: forall e. Encoding -> Offset -> Offset -> Buffer -> String
readString = readStringImpl <<< show

foreign import readStringImpl
  "function readStringImpl (enc) { \
  \  return function (start) { \
  \    return function (end) { \
  \      return function (buff) { \
  \        return buff.toString(enc, start, end); \
  \      } \
  \    } \
  \  } \
  \}":: String -> Offset -> Offset -> Buffer -> String
  
-- |
-- Reads the buffer as a string with the specified encoding.
--
toString :: forall e. Encoding -> Buffer -> String
toString = toStringImpl <<< show

foreign import toStringImpl
  "function toStringImpl (enc) { \
  \  return function (buff) { \
  \    return buff.toString(enc); \
  \  } \
  \}":: String -> Buffer -> String

-- |
-- Writes a numeric value to a buffer at the specified offset.
--
write :: forall e. BufferValueType -> Number -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
write = writeImpl <<< show

foreign import writeImpl
  "function writeImpl (ty) { \
  \  return function (value) { \
  \    return function (offset) { \
  \      return function (buf) { \
  \        buf['write' + ty](value, offset); \
  \        return {}; \
  \      }; \
  \    }; \
  \  }; \
  \}" :: forall e. String -> Number -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit

-- |
-- Writes octets from a string to a buffer at the specified offset. Multi-byte 
-- characters will not be written to the buffer if there is not enough capacity
-- to write them fully. The number of bytes written is returned.
--
writeString :: forall e. Encoding -> Offset -> Number -> String -> Buffer -> Eff (buffer :: BufferWrite | e) Number
writeString = writeStringImpl <<< show

foreign import writeStringImpl
  "function writeStringImpl (enc) { \
  \  return function (offset) { \
  \      return function (length) { \
  \        return function (value) { \
  \          return function (buff) { \
  \            return buff.write(value, offset, length, encoding); \
  \          }; \
  \        }; \
  \      }; \
  \  }; \
  \}" :: forall e. String -> Offset -> Number -> String -> Buffer -> Eff (buffer :: BufferWrite | e) Number

-- |
-- Creates an array of octets from a buffer's contents.
--
foreign import toArray
  "function toArray (buff) { \
  \  return buff.toJSON(); \
  \}" :: Buffer -> [Octet]

-- |
-- Reads an octet from a buffer at the specified offset.
--
foreign import getAtOffset
  "function getAtOffset (buff) { \
  \  return function (offset) { \
  \    var octet = buff[offset]; \
  \    return octet == null ? _ps.Data_Maybe.Nothing \
  \                         : _ps.Data_Maybe.Just(buff[i]); \
  \  } \
  \}" :: Offset -> Buffer -> Maybe Octet

-- |
-- Writes an octet in the buffer at the specified offset.
--
foreign import setAtOffset
  "function setAtOffset (value) { \
  \  return function (offset) { \
  \    return function (buff) { \
  \      buff[offset] = value; \
  \      return {}; \
  \    } \
  \  } \
  \}" :: forall e. Octet -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit

-- |
-- Returns the size of a buffer.
--
foreign import size
  "function size (buff) { \
  \  return buff.length; \
  \}" :: forall e. Buffer -> Number

-- |
-- Concatenates a list of buffers.
--
foreign import concat
  "function concat (buffs) { \
  \  return Buffer.concat(buffs); \
  \}" :: [Buffer] -> Buffer
  
-- |
-- Concatenates a list of buffers, combining them into a new buffer of the 
-- specified length.
--
foreign import concat'
  "function concat$prime (buffs) { \
  \  return function (totalLength) { \
  \    return Buffer.concat(buffs, totalLength); \
  \  } \
  \}" :: [Buffer] -> Number -> Buffer

-- |
-- Copies a section of a source buffer into a target buffer at the specified 
-- offset.
--
foreign import copy
  "function copy (srcStart) { \
  \  return function (srcEnd) { \
  \    return function (src) { \
  \      return function (targStart) { \
  \        return function (targ) { \
  \          return src.copy(targ, targStart, srcStart, strcEnd); \
  \        } \
  \      } \
  \    } \
  \  } \
  \}" :: Offset -> Offset -> Buffer -> Offset -> Buffer -> Buffer
  
-- |
-- Fills a range in a buffer with the specified octet.
--
foreign import fill
  "function fill (buff) { \
  \  return function (octet) { \
  \    return function (start) { \
  \      return function (end) { \
  \        buff.fill(octet, start, end); \
  \        return {}; \
  \      } \
  \    } \
  \  } \
  \}" :: forall e. Octet -> Offset -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
