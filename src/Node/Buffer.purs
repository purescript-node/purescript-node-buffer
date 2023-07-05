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
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, EffectFn5, runEffectFn1, runEffectFn3, runEffectFn4, runEffectFn5)
import Node.Buffer.Class (class MutableBuffer)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as Immutable
import Node.Buffer.Types (BufferValueType(..), Octet, Offset) as TypesExports
import Node.Buffer.Types (BufferValueType, Octet, Offset)
import Node.Encoding (Encoding, encodingToNode)
import Unsafe.Coerce (unsafeCoerce)

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

unsafeFreeze :: Buffer -> Effect ImmutableBuffer
unsafeFreeze = pure <<< unsafeCoerce

unsafeThaw :: ImmutableBuffer -> Effect Buffer
unsafeThaw = pure <<< unsafeCoerce

usingFromImmutable :: forall a. (ImmutableBuffer -> a) -> Buffer -> Effect a
usingFromImmutable f buf = f <$> unsafeFreeze buf

usingToImmutable :: forall a. (a -> ImmutableBuffer) -> a -> Effect Buffer
usingToImmutable f x = unsafeThaw $ f x

create :: Int -> Effect Buffer
create = usingToImmutable Immutable.create

freeze :: Buffer -> Effect ImmutableBuffer
freeze = runEffectFn1 freezeImpl

foreign import freezeImpl :: EffectFn1 Buffer ImmutableBuffer

thaw :: ImmutableBuffer -> Effect Buffer
thaw = runEffectFn1 thawImpl

foreign import thawImpl :: EffectFn1 ImmutableBuffer Buffer

fromArray :: Array Octet -> Effect Buffer
fromArray = usingToImmutable Immutable.fromArray

fromString :: String -> Encoding -> Effect Buffer
fromString s = usingToImmutable $ Immutable.fromString s

fromArrayBuffer :: ArrayBuffer -> Effect Buffer
fromArrayBuffer = usingToImmutable Immutable.fromArrayBuffer

toArrayBuffer :: Buffer -> Effect ArrayBuffer
toArrayBuffer = usingFromImmutable Immutable.toArrayBuffer

read :: BufferValueType -> Offset -> Buffer -> Effect Number
read t o = usingFromImmutable $ Immutable.read t o

readString :: Encoding -> Offset -> Offset -> Buffer -> Effect String
readString enc o o' = usingFromImmutable $ Immutable.readString enc o o'

toString :: Encoding -> Buffer -> Effect String
toString enc = usingFromImmutable $ Immutable.toString enc

write :: BufferValueType -> Number -> Offset -> Buffer -> Effect Unit
write ty value offset buf = runEffectFn4 writeInternal (show ty) value offset buf

foreign import writeInternal :: EffectFn4 String Number Offset Buffer Unit

writeString :: Encoding -> Offset -> Int -> String -> Buffer -> Effect Int
writeString enc offset len value buf =
  runEffectFn5 writeStringInternal (encodingToNode enc) offset len value buf

foreign import writeStringInternal :: EffectFn5 String Offset Int String Buffer Int

toArray :: Buffer -> Effect (Array Octet)
toArray = usingFromImmutable Immutable.toArray

getAtOffset :: Offset -> Buffer -> Effect (Maybe Octet)
getAtOffset o = usingFromImmutable $ Immutable.getAtOffset o

setAtOffset :: Octet -> Offset -> Buffer -> Effect Unit
setAtOffset val off buff = runEffectFn3 setAtOffsetImpl val off buff

foreign import setAtOffsetImpl :: EffectFn3 Octet Offset Buffer Unit

slice :: Offset -> Offset -> Buffer -> Buffer
slice = unsafeCoerce Immutable.slice

size :: Buffer -> Effect Int
size = usingFromImmutable Immutable.size

concat :: Array Buffer -> Effect Buffer
concat arrs = unsafeCoerce \_ -> Immutable.concat (unsafeCoerce arrs)

concat' :: Array Buffer -> Int -> Effect Buffer
concat' arrs n = unsafeCoerce \_ -> Immutable.concat' (unsafeCoerce arrs) n

copy :: Offset -> Offset -> Buffer -> Offset -> Buffer -> Effect Int
copy srcStart srcEnd src targStart targ = do
  runEffectFn5 copyImpl srcStart srcEnd src targStart targ

foreign import copyImpl :: EffectFn5 Offset Offset Buffer Offset Buffer Int

fill :: Octet -> Offset -> Offset -> Buffer -> Effect Unit
fill octet start end buf = do
  runEffectFn4 fillImpl octet start end buf

foreign import fillImpl :: EffectFn4 Octet Offset Offset Buffer Unit
