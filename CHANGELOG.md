# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v6.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v6.0.0) - 2019-07-24

* Add a `MutableBuffer` type class with instances for both `Effect` and `ST`, to allow mutating buffers in either monad, and potentially other monads too (#24, @Dretch)
* Remove the `Show Buffer` instance, as reading from a mutable buffer is effectful (@Dretch)
* Use `Number` for reading and writing with Buffers (#25, @hdgarrood)

## [v5.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v5.0.0) - 2018-05-26

Updated for PureScript 0.12

## [v4.1.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v4.1.0) - 2017-12-11

Add `fromArrayBuffer` (@matthewleon)

## [v4.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v4.0.0) - 2017-11-19

- Added `toArrayBuffer` (@matthewleon)

## [v3.0.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v3.0.1) - 2017-06-20

Fix an encoding issue in `writeString` (@justinwoo)

## [v3.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v3.0.0) - 2017-04-04

Updates for 0.11 (@anilanar)

## [v2.0.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v2.0.1) - 2017-02-10

- Fixed `getAtOffset` FFI implementation (@rightfold)
- Fixed documentation typo (@rightfold)

## [v2.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v2.0.0) - 2016-10-17

- Updated dependencies (@nwolverson)
- Added `latin1` encoding (@Risto-Stevcev)

## [v1.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v1.0.0) - 2016-06-06

- Compatibility with psc 0.9
- Better behaved `Show` instance, plus an `encodingToNode` function which is now recommended for use with node APIs instead of `show`.

## [v0.2.2](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.2.2) - 2016-04-03

Fix `byteLength`, which had a bad FFI declaration, so that it would throw an error every time it was called.

## [v0.2.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.2.1) - 2016-04-03

No code changes, this tag was just for publishing to Pursuit.

## [v0.2.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.2.0) - 2015-11-11

- Major important fixes. This is a breaking change as type signatures have changed. (@hdgarrood)

## [v0.1.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.1.1) - 2015-11-10

Release v0.1.1
Fix `write`, `writeString`, and `fill`, which used to immediately throw errors at
runtime upon use.

## [v0.1.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.1.0) - 2015-07-06

- Support for compiler version 0.7
- Use Int instead of Number where appropriate (which happens to be everywhere)

## [v0.0.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.0.1) - 2014-10-14

Initial versioned release

