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

create ∷ ∀ (h ∷ Region). Int → ST h (STBuffer h)
create = Internal.create

freeze ∷ ∀ (h ∷ Region). STBuffer h → ST h ImmutableBuffer
freeze = Internal.copyAll

unsafeFreeze ∷ ∀ (h ∷ Region). STBuffer h → ST h ImmutableBuffer
unsafeFreeze = Internal.unsafeFreeze

thaw ∷ ∀ (h ∷ Region). ImmutableBuffer → ST h (STBuffer h)
thaw = Internal.copyAll

unsafeThaw ∷ ∀ (h ∷ Region). ImmutableBuffer → ST h (STBuffer h)
unsafeThaw = Internal.unsafeThaw

fromArray ∷ ∀ (h ∷ Region). Array Int → ST h (STBuffer h)
fromArray = Internal.fromArray

fromString ∷ ∀ (h ∷ Region). String → Encoding → ST h (STBuffer h)
fromString = Internal.fromString

fromArrayBuffer ∷ ∀ (h ∷ Region). ArrayBuffer → ST h (STBuffer h)
fromArrayBuffer = Internal.fromArrayBuffer

toArrayBuffer ∷ ∀ (h ∷ Region). STBuffer h → ST h ArrayBuffer
toArrayBuffer = Internal.toArrayBuffer

read ∷ ∀ (h ∷ Region). BufferValueType → Int → STBuffer h → ST h Number
read = Internal.read

readString ∷ ∀ (h ∷ Region). Encoding → Int → Int → STBuffer h → ST h String
readString = Internal.readString

toString ∷ ∀ (h ∷ Region). Encoding → STBuffer h → ST h String
toString = Internal.toString

write ∷ ∀ (h ∷ Region). BufferValueType → Number → Int → STBuffer h → ST h Unit
write = Internal.write

writeString ∷ ∀ (h ∷ Region). Encoding → Int → Int → String → STBuffer h → ST h Int
writeString = Internal.writeString

toArray ∷ ∀ (h ∷ Region). STBuffer h → ST h (Array Int)
toArray = Internal.toArray

getAtOffset ∷ ∀ (h ∷ Region). Int → STBuffer h → ST h (Maybe Int)
getAtOffset = Internal.getAtOffset

setAtOffset ∷ ∀ (h ∷ Region). Int → Int → STBuffer h → ST h Unit
setAtOffset = Internal.setAtOffset

slice ∷ ∀ (h ∷ Region). Int → Int → STBuffer h → STBuffer h
slice = Internal.slice

size ∷ ∀ (h ∷ Region). STBuffer h → ST h Int
size = Internal.size

concat ∷ ∀ (h ∷ Region). Array (STBuffer h) → ST h (STBuffer h)
concat = Internal.concat

concat' ∷ ∀ (h ∷ Region). Array (STBuffer h) → Int → ST h (STBuffer h)
concat' = Internal.concat'

copy ∷ ∀ (h ∷ Region). Int → Int → STBuffer h → Int → STBuffer h → ST h Int
copy = Internal.copy

fill ∷ ∀ (h ∷ Region). Int → Int → Int → STBuffer h → ST h Unit
fill = Internal.fill
