module Node.Buffer.ST.Unsafe where

import Node.Buffer as Buffer
import Node.Buffer (Offset)
import Node.Buffer.ST (STBuffer)
import Unsafe.Coerce (unsafeCoerce)

-- | Creates a new buffer slice that acts like a window on the original buffer.
-- | Writing to the slice buffer updates the original buffer and vice-versa.
--
-- | This is considered unsafe as writing to a slice can result in action at a
-- | distance.
slice :: forall h. Offset -> Offset -> STBuffer h -> STBuffer h
slice = unsafeCoerce Buffer.slice
