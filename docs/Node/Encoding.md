## Module Node.Encoding

#### `Encoding`

``` purescript
data Encoding
  = ASCII
  | UTF8
  | UTF16LE
  | UCS2
  | Base64
  | Binary
  | Hex
```

##### Instances
``` purescript
instance showEncoding :: Show Encoding
```

#### `byteLength`

``` purescript
byteLength :: String -> Encoding -> Int
```


