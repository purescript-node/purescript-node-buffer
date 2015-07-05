## Module Node.Buffer

#### `Octet`

``` purescript
type Octet = Int
```

#### `Offset`

``` purescript
type Offset = Int
```

#### `Buffer`

``` purescript
data Buffer :: *
```

##### Instances
``` purescript
instance showBuffer :: Show Buffer
```

#### `BufferWrite`

``` purescript
data BufferWrite :: !
```

#### `BufferValueType`

``` purescript
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
```

##### Instances
``` purescript
instance showBufferValueType :: Show BufferValueType
```

#### `create`

``` purescript
create :: Int -> Buffer
```

#### `fromArray`

``` purescript
fromArray :: Array Octet -> Buffer
```

#### `fromString`

``` purescript
fromString :: String -> Encoding -> Buffer
```

#### `read`

``` purescript
read :: BufferValueType -> Offset -> Buffer -> Int
```

#### `readString`

``` purescript
readString :: Encoding -> Offset -> Offset -> Buffer -> String
```

#### `toString`

``` purescript
toString :: Encoding -> Buffer -> String
```

#### `write`

``` purescript
write :: forall e. BufferValueType -> Int -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
```

#### `writeString`

``` purescript
writeString :: forall e. Encoding -> Offset -> Int -> String -> Buffer -> Eff (buffer :: BufferWrite | e) Int
```

#### `toArray`

``` purescript
toArray :: Buffer -> Array Octet
```

#### `getAtOffset`

``` purescript
getAtOffset :: Offset -> Buffer -> Maybe Octet
```

#### `setAtOffset`

``` purescript
setAtOffset :: forall e. Octet -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
```

#### `size`

``` purescript
size :: Buffer -> Int
```

#### `concat`

``` purescript
concat :: Array Buffer -> Buffer
```

#### `concat'`

``` purescript
concat' :: Array Buffer -> Int -> Buffer
```

#### `copy`

``` purescript
copy :: Offset -> Offset -> Buffer -> Offset -> Buffer -> Buffer
```

#### `fill`

``` purescript
fill :: forall e. Octet -> Offset -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
```


