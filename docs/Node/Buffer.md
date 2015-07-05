## Module Node.Buffer

#### `Octet`

``` purescript
type Octet = Number
```

#### `Offset`

``` purescript
type Offset = Number
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
read :: BufferValueType -> Offset -> Buffer -> Number
```

#### `readString`

``` purescript
readString :: forall e. Encoding -> Offset -> Offset -> Buffer -> String
```

#### `toString`

``` purescript
toString :: forall e. Encoding -> Buffer -> String
```

#### `write`

``` purescript
write :: forall e. BufferValueType -> Number -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
```

#### `writeString`

``` purescript
writeString :: forall e. Encoding -> Offset -> Number -> String -> Buffer -> Eff (buffer :: BufferWrite | e) Number
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
size :: Buffer -> Number
```

#### `concat`

``` purescript
concat :: Array Buffer -> Buffer
```

#### `concat'`

``` purescript
concat' :: Array Buffer -> Number -> Buffer
```

#### `copy`

``` purescript
copy :: Offset -> Offset -> Buffer -> Offset -> Buffer -> Buffer
```

#### `fill`

``` purescript
fill :: forall e. Octet -> Offset -> Offset -> Buffer -> Eff (buffer :: BufferWrite | e) Unit
```


