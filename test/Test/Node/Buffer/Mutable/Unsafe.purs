module Test.Node.Buffer.Mutable.Unsafe (test) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Control.Monad.ST as ST
import Node.Buffer.Mutable (EffectBuffer, STBuffer, fromArray, setAtOffset, toArray)
import Node.Buffer.Mutable.Unsafe (class MutableBufferUnsafe, slice)
import Test.Assert (assertEqual)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

test :: Effect Unit
test = do
  log "Testing Node.Buffer.Mutable.Unsafe [EffectBuffer] ..."
  testMutableBufferUnsafe (Proxy :: Proxy EffectBuffer) identity

  log "Testing Node.Buffer.Mutable.Unsafe [STBuffer] ..."
  testMutableBufferUnsafe (Proxy :: Proxy (STBuffer _)) (unsafeCoerce ST.run >>> pure)

testMutableBufferUnsafe :: forall b e. Monad e => MutableBufferUnsafe b e =>
  Proxy b -> (forall a. e a -> Effect a) -> Effect Unit
testMutableBufferUnsafe _ run = do

  log " - slice"
  testSlice

  where
    testSlice :: Effect Unit
    testSlice = do
      {bufArr, bufSliceArr} <- run do
        buf <- fromArray [1, 2, 3, 4] :: e b
        let bufSlice = slice 1 3 buf :: b
        setAtOffset 42 1 bufSlice
        bufArr <- toArray buf
        bufSliceArr <- toArray bufSlice
        pure {bufArr, bufSliceArr}

      assertEqual {expected: [1, 2, 42, 4], actual: bufArr}
      assertEqual {expected: [2, 42], actual: bufSliceArr}
