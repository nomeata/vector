{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}

{-

Fusion tests for vector
=======================

This file tests whether fusion happens the way we want it.

It does so by creating a typical pipeline with a function. For example, to test
`map`, we write

   test_map f = V.toList . V.map f . V.fromList

Actually, we insert the `fuseHere` function, from the Canary module, in the
spots where we expect fusion to happen:

   test_map f = V.toList . fuseHere . V.map f . fuseHere . V.fromList

The `fuseHere` function has rules included that make it disappear if fusion
happens at this spot. See Canary.hs for more details.
So if `fuseHere` remains in the code, fusion did not happen as expected. We
check this using the `inspection-testing` library.

Even if `fuseHere` disappeared, which means that for example the
`unstream/stream` rule fired, we want to check that all of the constructors
of the `Step` data type (`Skip`, `Done`, `Yield`) have been eliminiated. We
test this again using `inspection-testing`.

-}

module Main where

import qualified Data.Vector as V
import Canary
import Test.Inspection
import Data.Vector.Fusion.Stream.Monadic (Step(..))
import Control.Monad
import qualified Language.Haskell.TH as TH

main :: IO ()
main = return ()

-- Testing pipelines for producers, transformers, consumers
p f = V.toList . fuseHere . f
t f = V.toList . fuseHere . f . fuseHere . V.fromList
c f = f . fuseHere . V.fromList
{-# INLINE t #-}
{-# INLINE c #-}
{-# INLINE p #-}

-- To help the type checker, avoid ambiguous Monad ctraints 
inIO :: IO a -> IO a
inIO = id

-- Length information

test_length = c V.length
test_null = c V.null

-- Indexing

test_bang = c (V.! 42)
test_safe_bang = c (V.!? 42)
test_head = c V.head
test_last = c V.last
test_unsafeIndex = c (`V.unsafeIndex` 42)
test_unsafeHead = c V.unsafeHead
test_unsafeLast = c V.unsafeLast

-- Monadic Indexing

test_indexM = inIO . c (`V.indexM` 42)
test_headM = inIO . c V.headM
test_lastM = inIO . c V.lastM
test_unsafeIndexM = inIO . c (`V.unsafeIndexM` 42)
test_unsafeHeadM = inIO . c V.unsafeHeadM
test_unsafeLastM = inIO . c V.unsafeLastM

-- Extracting subvectors (slicing)
test_slice = t (V.slice 23 42)
test_init = t V.init
test_tail = t V.tail
test_take = t (V.take 42)
test_drop = t (V.drop 42)
-- splitAt: hard to test
test_unsafeSlice = t (V.unsafeSlice 23 42)
test_unsafeInit = t V.unsafeInit
test_unsafeTail = t V.unsafeTail
test_unsafeTake = t (V.unsafeTake 42)
-- Does not actually fuse
-- test_unsafeDrop = t (V.unsafeDrop 42)

-- Initialisation

-- Does not fuse, as the ctant expression floats out
--test_empty = p (\(_::()) -> V.empty)
test_singleton = p V.singleton
test_replicate = p (V.replicate 5)
test_generate = p (V.generate 5)
test_iterateN = p (V.iterateN 5 (+1))

-- Monadic Initialisation
--
-- These don't fuse (no rules for unstreamM)

-- Unfolding

test_unfoldr x = p (V.unfoldr x)
test_unfoldrN x = p (V.unfoldrN 42 x)
-- ctructN and ctructrN cannot fuse

-- Enumeration

test_enumFromN (x::Integer) = p (V.enumFromN x)
test_enumFromStepN (x::Integer) y = p (V.enumFromStepN x y)
test_enumFromTo (x::Integer) = p (V.enumFromTo x)
test_enumFromThenTo (x::Integer) y = p (V.enumFromThenTo x y)

-- Concatenation

test_cons x = t (V.cons x)
test_snoc x = t (`V.snoc` x)
test_append_r x = t (x V.++)
test_append_l x = t (V.++ x)
test_concat = p V.concat

-- Bulk updates

-- bulk updates fuse as a consumers, but not as a producer
test_upd x = c (V.// x)
test_update_l x = c (`V.update` x)
test_update_r x = c (x `V.update`)
test_update__1 y z = c (\x -> V.update_ x y z)
test_update__2 y z = c (\x -> V.update_ y x z)
test_update__3 y z = c (\x -> V.update_ y z x)
test_unsafeUpd x = c (`V.unsafeUpd` x)
test_unsafeUpdate_l x = c (`V.unsafeUpdate` x)
test_unsafeUpdate_r x = c (x `V.unsafeUpdate`)
test_unsafeUpdate__1 y z = c (\x -> V.unsafeUpdate_ x y z)
test_unsafeUpdate__2 y z = c (\x -> V.unsafeUpdate_ y x z)
test_unsafeUpdate__3 y z = c (\x -> V.unsafeUpdate_ y z x)

-- Accumulations

test_accum f y = c (\x -> V.accum f x y)
test_accumulate_l f y = c (\x -> V.accumulate f x y)
test_accumulate_r f y = c (\x -> V.accumulate f y x)
test_accumulate__1 f y z = c (\x -> V.accumulate_ f x y z)
test_accumulate__2 f y z = c (\x -> V.accumulate_ f y x z)
test_accumulate__3 f y z = c (\x -> V.accumulate_ f y z x)
test_unsafeAccum f y = c (\x -> V.unsafeAccum f x y)
test_unsafeAccumulate_l f y  = c (\x -> V.unsafeAccumulate f x y)
test_unsafeAccumulate_r f y  = c (\x -> V.unsafeAccumulate f y x)
test_unsafeAccumulate__1 f y z = c (\x -> V.unsafeAccumulate_ f x y z)
test_unsafeAccumulate__2 f y z = c (\x -> V.unsafeAccumulate_ f y x z)
test_unsafeAccumulate__3 f y z = c (\x -> V.unsafeAccumulate_ f y z x)

-- Permutations

-- reverse is only a good producer, not a good consumer
test_reverse = p V.reverse
-- backpermute is only a good consumer in the second argument,
-- but not the first and not a good producer
test_backpermute y = c (V.backpermute y)
test_unsafeBackpermute y = c (V.unsafeBackpermute y)

-- Elementwise operations

test_indexed = t V.indexed
test_map f = t (V.map f)
test_imap f = t (V.imap f)
test_concatMap f = t (V.concatMap f)
-- No deep fusion?
-- test_concatMap_deep f = t (V.concatMap (\ x -> fuseHere (f x)))

-- ... much more to come ...

fmap (concat . reverse)$ forM
  [ 'test_head
  , 'test_null
  , 'test_bang
  , 'test_safe_bang
  , 'test_head
  , 'test_last
  , 'test_unsafeIndex
  , 'test_unsafeHead
  , 'test_unsafeLast
  , 'test_indexM
  , 'test_headM
  , 'test_lastM
  , 'test_unsafeIndexM
  , 'test_unsafeHeadM
  , 'test_unsafeLastM
  , 'test_slice
  , 'test_init
  , 'test_tail
  , 'test_take
  , 'test_drop
  , 'test_unsafeSlice
  , 'test_unsafeInit
  , 'test_unsafeTail
  , 'test_unsafeTake
--  , 'test_unsafeDrop
--  , 'test_empty
  , 'test_singleton
  , 'test_replicate
  , 'test_generate
  , 'test_iterateN
  , 'test_unfoldr
  , 'test_unfoldrN
  , 'test_enumFromN
  , 'test_enumFromStepN
  , 'test_enumFromTo
  , 'test_enumFromThenTo
  , 'test_cons
  , 'test_cons
  , 'test_snoc
  , 'test_append_r
  , 'test_append_l
  , 'test_concat
  , 'test_upd
  , 'test_update_l
  , 'test_update_r
  , 'test_update__1
  , 'test_update__2
  , 'test_update__3
  , 'test_unsafeUpd
  , 'test_unsafeUpdate_l
  , 'test_unsafeUpdate_r
  , 'test_unsafeUpdate__1
  , 'test_unsafeUpdate__2
  , 'test_unsafeUpdate__3
  , 'test_accum
  , 'test_accumulate_l
  , 'test_accumulate_r
  , 'test_accumulate__1
  , 'test_accumulate__2
  , 'test_accumulate__3
  , 'test_unsafeAccum
  , 'test_unsafeAccumulate_l
  , 'test_unsafeAccumulate_r
  , 'test_unsafeAccumulate__1
  , 'test_unsafeAccumulate__2
  , 'test_unsafeAccumulate__3
  , 'test_reverse
  , 'test_backpermute
  , 'test_unsafeBackpermute
  , 'test_indexed
  , 'test_map
  , 'test_imap
  , 'test_concatMap
  ] $ \thn -> inspect
    (mkObligation thn (NoUseOf ['fuseHere, 'Yield, 'Skip, 'Done]))
    { testName = Just (TH.nameBase thn) }

