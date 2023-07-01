module Node.Buffer.ST
  ( STBuffer
  , run
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

import Control.Monad.ST (ST, Region)
import Control.Monad.ST as ST
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Node.Buffer (BufferValueType)
import Node.Buffer.Class (class MutableBuffer)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Internal as Internal
import Node.Encoding (Encoding)

-- | A reference to a mutable buffer for use with `ST`
-- |
-- | The type parameter represents the memory region which the buffer belongs to.
foreign import data STBuffer :: Region -> Type

-- | Runs an effect creating an `STBuffer` then freezes the buffer and returns
-- | it, without unneccessary copying.
run :: (forall h. ST h (STBuffer h)) -> ImmutableBuffer
run st = ST.run (st >>= unsafeFreeze)

instance mutableBufferST :: MutableBuffer (STBuffer h) (ST h) where
  create = create
  freeze = freeze
  unsafeFreeze = unsafeFreeze
  thaw = thaw
  unsafeThaw = unsafeThaw
  fromArray = fromArray
  fromString = fromString
  fromArrayBuffer = fromArrayBuffer
  toArrayBuffer = toArrayBuffer
  read = read
  readString = readString
  toString = toString
  write = write
  writeString = writeString
  toArray = toArray
  getAtOffset = getAtOffset
  setAtOffset = setAtOffset
  slice = slice
  size = size
  concat = concat
  concat' = concat'
  copy = copy
  fill = fill

create :: forall h. Int -> ST h (STBuffer h)
create = Internal.create

freeze :: forall h. STBuffer h -> ST h ImmutableBuffer
freeze = Internal.copyAll

unsafeFreeze :: forall h. STBuffer h -> ST h ImmutableBuffer
unsafeFreeze = Internal.unsafeFreeze

thaw :: forall h. ImmutableBuffer -> ST h (STBuffer h)
thaw = Internal.copyAll

unsafeThaw :: forall h. ImmutableBuffer -> ST h (STBuffer h)
unsafeThaw = Internal.unsafeThaw

fromArray :: forall h. Array Int -> ST h (STBuffer h)
fromArray = Internal.fromArray

fromString :: forall h. String -> Encoding -> ST h (STBuffer h)
fromString = Internal.fromString

fromArrayBuffer :: forall h. ArrayBuffer -> ST h (STBuffer h)
fromArrayBuffer = Internal.fromArrayBuffer

toArrayBuffer :: forall h. STBuffer h -> ST h ArrayBuffer
toArrayBuffer = Internal.toArrayBuffer

read :: forall h. BufferValueType -> Int -> STBuffer h -> ST h Number
read = Internal.read

readString :: forall h. Encoding -> Int -> Int -> STBuffer h -> ST h String
readString = Internal.readString

toString :: forall h. Encoding -> STBuffer h -> ST h String
toString = Internal.toString

write :: forall h. BufferValueType -> Number -> Int -> STBuffer h -> ST h Unit
write = Internal.write

writeString :: forall h. Encoding -> Int -> Int -> String -> STBuffer h -> ST h Int
writeString = Internal.writeString

toArray :: forall h. STBuffer h -> ST h (Array Int)
toArray = Internal.toArray

getAtOffset :: forall h. Int -> STBuffer h -> ST h (Maybe Int)
getAtOffset = Internal.getAtOffset

setAtOffset :: forall h. Int -> Int -> STBuffer h -> ST h Unit
setAtOffset = Internal.setAtOffset

slice :: forall h. Int -> Int -> STBuffer h -> STBuffer h
slice = Internal.slice

size :: forall h. STBuffer h -> ST h Int
size = Internal.size

concat :: forall h. Array (STBuffer h) -> ST h (STBuffer h)
concat = Internal.concat

concat' :: forall h. Array (STBuffer h) -> Int -> ST h (STBuffer h)
concat' = Internal.concat'

copy :: forall h. Int -> Int -> STBuffer h -> Int -> STBuffer h -> ST h Int
copy = Internal.copy

fill :: forall h. Int -> Int -> Int -> STBuffer h -> ST h Unit
fill = Internal.fill
