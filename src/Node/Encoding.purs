-- | Note: encodings that are aliases of another one
-- | are not supported below as a way to prevent confusion.
module Node.Encoding
  ( Encoding(..)
  , encodingToNode
  , byteLength
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)

data Encoding
  = ASCII
  | UTF8
  -- | Note: the `ucs2` and `ucs-2` encodings are aliases to this one.
  | UTF16LE
  | Base64
  | Base64Url
  -- | Note: the `binary` encoding is an alias to this one.
  | Latin1
  | Hex

instance showEncoding :: Show Encoding where
  show ASCII = "ASCII"
  show UTF8 = "UTF8"
  show UTF16LE = "UTF16LE"
  show Base64 = "Base64"
  show Base64Url = "Base64Url"
  show Latin1 = "Latin1"
  show Hex = "Hex"

-- | Convert an `Encoding` to a `String` in the format expected by Node.js
-- | APIs.
encodingToNode :: Encoding -> String
encodingToNode ASCII = "ascii"
encodingToNode UTF8 = "utf8"
encodingToNode UTF16LE = "utf16le"
encodingToNode Base64 = "base64"
encodingToNode Base64Url = "base64url"
encodingToNode Latin1 = "latin1"
encodingToNode Hex = "hex"

foreign import byteLengthImpl :: Fn2 (String) (String) (Int)

byteLength :: String -> Encoding -> Int
byteLength str enc = runFn2 byteLengthImpl str (encodingToNode enc)
