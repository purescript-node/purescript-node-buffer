-- | Mutable buffers and associated operations.
module Node.Buffer
  ( Buffer
  , module TypesExports
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

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Effect (Effect)
import Node.Buffer.Class (class MutableBuffer)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Internal as Internal
import Node.Buffer.Types (BufferValueType(..), Octet, Offset) as TypesExports
import Node.Buffer.Types (BufferValueType)
import Node.Encoding (Encoding)

-- | A reference to a mutable buffer for use with `Effect`
foreign import data Buffer :: Type

instance mutableBufferEffect :: MutableBuffer Buffer Effect where
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

create ∷ Int → Effect Buffer
create = Internal.create

freeze ∷ Buffer → Effect ImmutableBuffer
freeze = Internal.copyAll

unsafeFreeze ∷ Buffer → Effect ImmutableBuffer
unsafeFreeze = Internal.unsafeFreeze

thaw ∷ ImmutableBuffer → Effect Buffer
thaw = Internal.copyAll

unsafeThaw ∷ ImmutableBuffer → Effect Buffer
unsafeThaw = Internal.unsafeThaw

fromArray ∷ Array Int → Effect Buffer
fromArray = Internal.fromArray

fromString ∷ String → Encoding → Effect Buffer
fromString = Internal.fromString

fromArrayBuffer ∷ ArrayBuffer → Effect Buffer
fromArrayBuffer = Internal.fromArrayBuffer

toArrayBuffer ∷ Buffer → Effect ArrayBuffer
toArrayBuffer = Internal.toArrayBuffer

read ∷ BufferValueType → Int → Buffer → Effect Number
read = Internal.read

readString ∷ Encoding → Int → Int → Buffer → Effect String
readString = Internal.readString

toString ∷ Encoding → Buffer → Effect String
toString = Internal.toString

write ∷ BufferValueType → Number → Int → Buffer → Effect Unit
write = Internal.write

writeString ∷ Encoding → Int → Int → String → Buffer → Effect Int
writeString = Internal.writeString

toArray ∷ Buffer → Effect (Array Int)
toArray = Internal.toArray

getAtOffset ∷ Int → Buffer → Effect (Maybe Int)
getAtOffset = Internal.getAtOffset

setAtOffset ∷ Int → Int → Buffer → Effect Unit
setAtOffset = Internal.setAtOffset

slice ∷ Int → Int → Buffer → Buffer
slice = Internal.slice

size ∷ Buffer → Effect Int
size = Internal.size

concat ∷ Array Buffer → Effect Buffer
concat = Internal.concat

concat' ∷ Array Buffer → Int → Effect Buffer
concat' = Internal.concat'

copy ∷ Int → Int → Buffer → Int → Buffer → Effect Int
copy = Internal.copy

fill ∷ Int → Int → Int → Buffer → Effect Unit
fill = Internal.fill
