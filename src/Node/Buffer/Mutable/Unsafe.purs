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

class MutableBuffer b e <= MutableBufferUnsafe b e | b -> e, e -> b where

  -- | Creates a new buffer slice that acts like a window on the original buffer.
  -- | Writing to the slice buffer updates the original buffer and vice-versa.
  -- |
  -- | This is considered unsafe as writing to a slice can result in action at a
  -- | distance.
  slice :: Offset -> Offset -> b -> b

instance mutableBufferUnsafeEffect :: MutableBufferUnsafe EffectBuffer Effect where
  slice = sliceImpl

instance mutableBufferUnsafeST :: MutableBufferUnsafe (STBuffer h) (ST h) where
  slice = sliceImpl

sliceImpl :: forall b. Offset -> Offset -> b -> b
sliceImpl = unsafeCoerce Buffer.slice
