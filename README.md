# Module Documentation

## Module Node.Buffer

### Types

    data Buffer :: *

    data BufferValueType where
      UInt8 :: BufferValueType
      UInt16LE :: BufferValueType
      UInt16BE :: BufferValueType
      UInt32LE :: BufferValueType
      UInt32BE :: BufferValueType
      Int8 :: BufferValueType
      Int16LE :: BufferValueType
      Int16BE :: BufferValueType
      Int32LE :: BufferValueType
      Int32BE :: BufferValueType
      FloatLE :: BufferValueType
      FloatBE :: BufferValueType
      DoubleLE :: BufferValueType
      DoubleBE :: BufferValueType

    data BufferWrite :: !

    type Octet  = Number

    type Offset  = Number


### Type Class Instances

    instance showBuffer :: Show Buffer

    instance showBufferValueType :: Show BufferValueType


### Values

    concat :: [Buffer] -> Buffer

    concat' :: [Buffer] -> Number -> Buffer

    copy :: Offset -> Offset -> Buffer -> Offset -> Buffer -> Buffer

    create :: Number -> Buffer

    fill :: forall e. Octet -> Offset -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit

    fromArray :: [Octet] -> Buffer

    fromString :: String -> Encoding -> Buffer

    getAtOffset :: Offset -> Buffer -> Maybe Octet

    read :: BufferValueType -> Offset -> Buffer -> Number

    readString :: forall e. Encoding -> Offset -> Offset -> Buffer -> String

    setAtOffset :: forall e. Octet -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit

    size :: forall e. Buffer -> Number

    toArray :: Buffer -> [Octet]

    toString :: forall e. Encoding -> Buffer -> String

    write :: forall e. BufferValueType -> Number -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit

    writeString :: forall e. Encoding -> Offset -> Number -> String -> Buffer -> Eff (buffer :: BufferWrite | e) Number


## Module Node.Buffer.Unsafe

### Values

    slice :: Offset -> Offset -> Buffer -> Buffer


## Module Node.Encoding

### Types

    data Encoding where
      ASCII :: Encoding
      UTF8 :: Encoding
      UTF16LE :: Encoding
      UCS2 :: Encoding
      Base64 :: Encoding
      Binary :: Encoding
      Hex :: Encoding


### Type Class Instances

    instance showEncoding :: Show Encoding


### Values

    byteLength :: String -> Encoding -> Number