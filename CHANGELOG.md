# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:


## [v9.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v9.0.0) - 2022-06-05

Breaking changes:
- Expose Buffer API using typeclass-less API witout removing typeclass API (#53 by @JordanMartinez)

  Previously, compiler would fail to infer the type of `Buffer.create 1` as `Effect Buffer` 
  because the Buffer API was exposed only via the multiparameter typeclass `MonadBuffer`.
  Due to the functional dependency between the two parameters, the monadic type cannot be inferred
  until the buffer type is known (either `Buffer` or `STBuffer`).:
  ```purs
  import Node.Buffer as Buffer

  -- Example 1
  main :: Effect Unit
  main = do
    x <- Buffer.create 1 -- compiler: is this `Int -> Effect Buffer` or `Int -> ST h (STBuffer h)?
    pure unit
  ```

  The workaround was to add a type annotation, indicating the `x` is a `Buffer`:
  ```purs
  import Node.Buffer as Buffer
  
  -- Example 2
  main :: Effect Unit
  main = do
    x :: Buffer <- Buffer.create 1 -- compiler: Oh! It's `Int -> Effect Buffer`
    pure unit
  ```

  This change does not break anyone's code if one was using a `create` (or another such typeclass member)
  to get `Int -> Effect Buffer`. Rather, such users can now drop the `:: Buffer` annotation 
  (i.e. Example 1 above now compiles).

  If one was using `create` to get `forall m buf. MonadBuffer buf m => Int -> m buf`,
  then one will need to update their imports:
  ```diff
  -import Node.Buffer (class MonadBuffer)
  +import Node.Buffer.Class (class MonadBuffer)
  ```

New features:
- Added the following APIs (#55 by @JordanMartinez)

  - `Buffer.alloc`, `Buffer.allocUnsafe`, `Buffer.allocUnsafeSlow`
  - `Buffer.poolSize`, `Buffer.setPoolSize`
  - `buffer.swap16`, `buffer.swap32`, `buffer.swap64`
  - `buffer.compare`: https://nodejs.org/docs/latest-v18.x/api/buffer.html#bufcomparetarget-targetstart-targetend-sourcestart-sourceend
  - `buffer.toString(encoding, start, end)`: https://nodejs.org/docs/latest-v18.x/api/buffer.html#buftostringencoding-start-end
  - `buffer.transcode(buf, from, to)`
  - constants: 
    - `INSPECT_MAX_BYTES`: https://nodejs.org/docs/latest-v18.x/api/buffer.html#bufferinspect_max_bytes
    - `MAX_LENGTH`: https://nodejs.org/docs/latest-v18.x/api/buffer.html#bufferconstantsmax_length
    - `MAX_STRING_LENGTH`: https://nodejs.org/docs/latest-v18.x/api/buffer.html#bufferconstantsmax_string_length
- Added a new data constructor for `Encoding`: `Base64Url` (#56 by @JordanMartinez)

Bugfixes:

Other improvements:
- Format code with `purs-tidy`; enforce in CI (#52 by @JordanMartinez)
- Update FFI to use uncurried functions (#54 by @JordanMartinez)
- Removed `Internal.purs` file (#54 by @JordanMartinez)
- Bumped CI's node version to `lts/*` (#55/#57 by @JordanMartinez)
- Updated CI `actions/checkout` and `actions/setup-nodee` to `v3` (#55 by @JordanMartinez)

## [v8.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v8.0.0) - 2022-04-27

Breaking changes:

- Update project and deps to PureScript v0.15.0 (#46 by @sigma-andex and @JordanMartinez)

New features:

Bugfixes:

Other improvements:

## [v7.0.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v7.0.1) - 2021-05-06

Other improvements:
- Fix warnings revealed by v0.14.1 PS release (#44 by @JordanMartinez)

## [v7.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v7.0.0) - 2021-02-26

Breaking changes:
- Updated code for PureScript 0.14 and dropped the `proxy` dependency as the `proxy` library has been migrated into `prelude` (#39).

Other improvements:
- Removed primes from the `concat'` function in FFI in preparation for ES modules support (#36)
- Migrated CI to use GitHub Actions and updated installation instructions to use Spago (#38)
- Stopped returning empty objects in foreign implementations for functions which return `Unit` for a small performance benefit (#40)
- Added a CHANGELOG.md file and pull request template to the repository (#41)

## [v6.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v6.0.0) - 2019-07-24

* Add a `MutableBuffer` type class with instances for both `Effect` and `ST`, to allow mutating buffers in either monad, and potentially other monads too (#24, @Dretch)
* Remove the `Show Buffer` instance, as reading from a mutable buffer is effectful (@Dretch)
* Use `Number` for reading and writing with Buffers (#25, @hdgarrood)

## [v5.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v5.0.0) - 2018-05-26

- Updated for PureScript 0.12

## [v4.1.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v4.1.0) - 2017-12-11

- Added `fromArrayBuffer` (@matthewleon)

## [v4.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v4.0.0) - 2017-11-19

- Added `toArrayBuffer` (@matthewleon)

## [v3.0.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v3.0.1) - 2017-06-20

- Fixed an encoding issue in `writeString` (@justinwoo)

## [v3.0.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v3.0.0) - 2017-04-04

- Updated for 0.11 (@anilanar)

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

- Fixed `byteLength`, which had a bad FFI declaration, so that it would throw an error every time it was called.

## [v0.2.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.2.1) - 2016-04-03

- No code changes, this tag was just for publishing to Pursuit.

## [v0.2.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.2.0) - 2015-11-11

- Major important fixes. This is a breaking change as type signatures have changed. (@hdgarrood)

## [v0.1.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.1.1) - 2015-11-10

- Fixed `write`, `writeString`, and `fill`, which used to immediately throw errors at runtime upon use.

## [v0.1.0](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.1.0) - 2015-07-06

- Support for compiler version 0.7
- Use Int instead of Number where appropriate (which happens to be everywhere)

## [v0.0.1](https://github.com/purescript-node/purescript-node-buffer/releases/tag/v0.0.1) - 2014-10-14

- Initial versioned release
