## Module Node.Buffer

#### `Octet`

``` purescript
type Octet = Int
```

Type synonym indicating the value should be an octet (0-255). If the value
provided is outside this range it will be used as modulo 255.

#### `Offset`

``` purescript
type Offset = Int
```

Type synonym indicating the value refers to an offset in a buffer.

#### `Buffer`

``` purescript
data Buffer :: *
```

An instance of Node's Buffer class.

##### Instances
``` purescript
Show Buffer
```

#### `BUFFER`

``` purescript
data BUFFER :: !
```

Effect for buffer creation, reading, or writing.

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

Enumeration of the numeric types that can be written to a buffer.

##### Instances
``` purescript
Show BufferValueType
```

#### `create`

``` purescript
create :: forall e. Int -> Eff (buffer :: BUFFER | e) Buffer
```

Creates a new buffer of the specified size.

#### `fromArray`

``` purescript
fromArray :: forall e. Array Octet -> Eff (buffer :: BUFFER | e) Buffer
```

Creates a new buffer from an array of octets, sized to match the array.

#### `fromString`

``` purescript
fromString :: forall e. String -> Encoding -> Eff (buffer :: BUFFER | e) Buffer
```

Creates a new buffer from a string with the specified encoding, sized to
match the string.

#### `read`

``` purescript
read :: forall e. BufferValueType -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Int
```

Reads a numeric value from a buffer at the specified offset.

#### `readString`

``` purescript
readString :: forall e. Encoding -> Offset -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) String
```

Reads a section of a buffer as a string with the specified encoding.

#### `toString`

``` purescript
toString :: forall e. Encoding -> Buffer -> Eff (buffer :: BUFFER | e) String
```

Reads the buffer as a string with the specified encoding.

#### `write`

``` purescript
write :: forall e. BufferValueType -> Int -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Unit
```

Writes a numeric value to a buffer at the specified offset.

#### `writeString`

``` purescript
writeString :: forall e. Encoding -> Offset -> Int -> String -> Buffer -> Eff (buffer :: BUFFER | e) Int
```

Writes octets from a string to a buffer at the specified offset. Multi-byte
characters will not be written to the buffer if there is not enough capacity
to write them fully. The number of bytes written is returned.

#### `toArray`

``` purescript
toArray :: forall e. Buffer -> Eff (buffer :: BUFFER | e) (Array Octet)
```

Creates an array of octets from a buffer's contents.

#### `getAtOffset`

``` purescript
getAtOffset :: forall e. Offset -> Buffer -> Eff (buffer :: BUFFER | e) (Maybe Octet)
```

Reads an octet from a buffer at the specified offset.

#### `setAtOffset`

``` purescript
setAtOffset :: forall e. Octet -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Unit
```

Writes an octet in the buffer at the specified offset.

#### `size`

``` purescript
size :: forall e. Buffer -> Eff (buffer :: BUFFER | e) Int
```

Returns the size of a buffer.

#### `concat`

``` purescript
concat :: forall e. Array Buffer -> Eff (buffer :: BUFFER | e) Buffer
```

Concatenates a list of buffers.

#### `concat'`

``` purescript
concat' :: forall e. Array Buffer -> Int -> Eff (buffer :: BUFFER | e) Buffer
```

Concatenates a list of buffers, combining them into a new buffer of the
specified length.

#### `copy`

``` purescript
copy :: forall e. Offset -> Offset -> Buffer -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Int
```

Copies a section of a source buffer into a target buffer at the specified
offset, and returns the number of octets copied.

#### `fill`

``` purescript
fill :: forall e. Octet -> Offset -> Offset -> Buffer -> Eff (buffer :: BUFFER | e) Unit
```

Fills a range in a buffer with the specified octet.


