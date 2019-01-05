-- | Unsafe operations on mutable buffers.
module Node.Buffer.Mutable.Unsafe
 ( class MutableBufferUnsafe
 , slice
 ) where

import Effect (Effect)
import Control.Monad.ST (ST, kind Region)
import Node.Buffer as Buffer
import Node.Buffer (Offset)
import Node.Buffer.Mutable (class MutableBuffer, EffectBuffer, STBuffer)
import Unsafe.Coerce (unsafeCoerce)

class MutableBuffer buf m <= MutableBufferUnsafe buf m | buf -> m, m -> buf where

  -- | Creates a new buffer slice that acts like a window on the original buffer.
  -- | Writing to the slice buffer updates the original buffer and vice-versa.
  -- |
  -- | This is considered unsafe as writing to a slice can result in action at a
  -- | distance.
  slice :: Offset -> Offset -> buf -> buf

instance mutableBufferUnsafeEffect :: MutableBufferUnsafe EffectBuffer Effect where
  slice = sliceImpl

instance mutableBufferUnsafeST :: MutableBufferUnsafe (STBuffer h) (ST h) where
  slice = sliceImpl

sliceImpl :: forall buf. Offset -> Offset -> buf -> buf
sliceImpl = unsafeCoerce Buffer.slice
