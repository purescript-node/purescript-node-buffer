module Node.Encoding
  ( Encoding (..)
  , byteLength
  ) where

import Prelude

data Encoding
  = ASCII
  | UTF8
  | UTF16LE
  | UCS2
  | Base64
  | Binary
  | Hex

instance showEncoding :: Show Encoding where
  show ASCII   = "ascii"
  show UTF8    = "utf8"
  show UTF16LE = "utf16le"
  show UCS2    = "ucs2"
  show Base64  = "base64"
  show Binary  = "binary"
  show Hex     = "hex"

foreign import byteLengthImpl :: String -> String -> Number

byteLength :: String -> Encoding -> Number
byteLength str enc = byteLengthImpl str (show enc)
