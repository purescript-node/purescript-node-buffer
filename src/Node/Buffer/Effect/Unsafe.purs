module Node.Buffer.Effect.Unsafe where

import Node.Buffer as Buffer
import Node.Buffer (Offset)
import Node.Buffer.Effect (EffectBuffer)
import Unsafe.Coerce (unsafeCoerce)

-- | Creates a new buffer slice that acts like a window on the original buffer.
-- | Writing to the slice buffer updates the original buffer and vice-versa.
--
-- | This is considered unsafe as writing to a slice can result in action at a
-- | distance.
slice :: Offset -> Offset -> EffectBuffer -> EffectBuffer
slice = unsafeCoerce Buffer.slice
