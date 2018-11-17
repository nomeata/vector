{- |

This module provide the function 'fuseHere', which can be inserted into a
pipeline of vector-processing functions. It also contains copies of all the
fusion-related rewrite rules from "Data.Vector.Generic", with 'fuseHere'
inserted. This way, if fusion happens at this point, the 'fuseHere' function
disappears.

Having to maintain a complete copy of all the rewrite rules is a big downsid of
this approach, and a better way would be appreciated.

-}
module Canary (fuseHere) where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.New as New
import           Data.Vector.Fusion.Stream.Monadic ( Stream )
import qualified Data.Vector.Fusion.Bundle as Bundle
import           Data.Vector.Fusion.Bundle ( Bundle, MBundle, lift, inplace )
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle

-- | Put this function into vector pipelines where you want them to fuse
fuseHere :: a -> a
fuseHere = id
{-# NOINLINE fuseHere #-}

-- | We need to copy all fusion rules here, with fuseHere inserted in the right
-- spot.

{-# RULES

"(!)/fuseHere/unstream [Vector]" forall i s.
  fuseHere (V.new (New.unstream s)) V.! i = s Bundle.!! i

"(!?)/fuseHere/unstream [Vector]" forall i s.
  fuseHere (V.new (New.unstream s)) V.!? i = s Bundle.!? i

"head/fuseHere/unstream [Vector]" forall s.
  V.head (fuseHere (V.new (New.unstream s))) = Bundle.head s

"last/fuseHere/unstream [Vector]" forall s.
  V.last (fuseHere (V.new (New.unstream s))) = Bundle.last s

"unsafeIndex/fuseHere/unstream [Vector]" forall i s.
  V.unsafeIndex (fuseHere (V.new (New.unstream s))) i = s Bundle.!! i

"unsafeHead/fuseHere/unstream [Vector]" forall s.
  V.unsafeHead (fuseHere (V.new (New.unstream s))) = Bundle.head s

"unsafeLast/fuseHere/unstream [Vector]" forall s.
  V.unsafeLast (fuseHere (V.new (New.unstream s))) = Bundle.last s  #-}

{-# RULES

"indexM/fuseHere/unstream [Vector]" forall s i.
  V.indexM (fuseHere (V.new (New.unstream s))) i = lift s MBundle.!! i

"headM/fuseHere/unstream [Vector]" forall s.
  V.headM (fuseHere (V.new (New.unstream s))) = MBundle.head (lift s)

"lastM/fuseHere/unstream [Vector]" forall s.
  V.lastM (fuseHere (V.new (New.unstream s))) = MBundle.last (lift s)

"unsafeIndexM/fuseHere/unstream [Vector]" forall s i.
  V.unsafeIndexM (fuseHere (V.new (New.unstream s))) i = lift s MBundle.!! i

"unsafeHeadM/fuseHere/unstream [Vector]" forall s.
  V.unsafeHeadM (fuseHere (V.new (New.unstream s))) = MBundle.head (lift s)

"unsafeLastM/fuseHere/unstream [Vector]" forall s.
  V.unsafeLastM (fuseHere (V.new (New.unstream s))) = MBundle.last (lift s)   #-}

{-# RULES

"slice/fuseHere/new [Vector]" forall i n p.
  V.slice i n (fuseHere (V.new p)) = V.new (New.slice i n p)

"init/fuseHere/new [Vector]" forall p.
  V.init (fuseHere (V.new p)) = V.new (New.init p)

"tail/fuseHere/new [Vector]" forall p.
  V.tail (fuseHere (V.new p)) = V.new (New.tail p)

"take/fuseHere/new [Vector]" forall n p.
  V.take n (fuseHere (V.new p)) = V.new (New.take n p)

"drop/fuseHere/new [Vector]" forall n p.
  V.drop n (fuseHere (V.new p)) = V.new (New.drop n p)

"unsafeSlice/fuseHere/new [Vector]" forall i n p.
  V.unsafeSlice i n (fuseHere (V.new p)) = V.new (New.unsafeSlice i n p)

"unsafeInit/fuseHere/new [Vector]" forall p.
  V.unsafeInit (fuseHere (V.new p)) = V.new (New.unsafeInit p)

"unsafeTail/fuseHere/new [Vector]" forall p.
  V.unsafeTail (fuseHere (V.new p)) = V.new (New.unsafeTail p)   #-}


{-# RULES

"stream/fuseHere/unstream [Vector]" forall s.
  V.stream (fuseHere (V.new (New.unstream s))) = s

"New.unstream/fuseHere/stream [Vector]" forall v.
  New.unstream (fuseHere (V.stream v)) = V.clone v

"clone/fuseHere/new [Vector]" forall p.
  V.clone (fuseHere (V.new p)) = p

"inplace [Vector]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a) g m.
  New.unstream (inplace f g (V.stream (V.new m))) = New.transform f g m

"uninplace [Vector]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a) g m.
  V.stream (V.new (New.transform f g m)) = inplace f g (V.stream (V.new m))
#-}


