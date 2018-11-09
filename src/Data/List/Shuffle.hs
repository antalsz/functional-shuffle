{-|
Module      : Data.List.Shuffle
Description : Functional list shuffling with Heinrich Apfelmus’s “merge shuffle” algorithm
Copyright   : Copyright © 2018 Antal Spector-Zabusky
License     : BSD3
Maintainer  : Antal Spector-Zabusky <antal.b.sz@gmail.com>

This module (and\/or this package) implements Heinrich Apfelmus’s “merge shuffle”
algorithm from his blog post
<https://apfelmus.nfshost.com/articles/random-permutations.html “Random Permutations and Sorting”>
(Feb 14, 2009).
-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE BangPatterns #-}

module Data.List.Shuffle (
  -- * Shuffle a list
  shuffle,
  -- * Utility functions
  -- ** List manipulation
  halve,
  -- ** Shuffling
  riffle, bridge
) where

import Control.Monad.Random

-- |Split a list into two non-contiguous halves.  The first returned half is all
-- the elements at even positions; the second is all the elements at odd
-- positions.  If the list has an odd number of elements, the first list will be
-- one element longer.
--
-- >>> halve [0..4]
-- ([0,2,4],[1,3])
-- >>> halve [0..9]
-- ([0,2,4,6,8],[1,3,5,7,9])
-- >>> halve [0]
-- ([0],[])
-- >>> halve []
-- ([],[])
halve :: [a] -> ([a],[a])
halve []     = ([],[])
halve (z:zs) = let (xs,ys) = halve zs
               in (z:ys, xs)

-- |Given two lists and their lengths, produce a random interleaving of the two
-- lists, uniformly at random.  An interleaving of two lists contains all the
-- elements from both lists, where each list’s elements are kept in order.
--
-- >>> riffle [1,2,3,4,5] 5 [60,70,80,90] 4
-- [1,60,70,80,2,3,90,4,5]
-- >>> riffle [1,2,3,4,5] 5 [60,70,80,90] 4
-- [60,70,80,1,2,3,4,90,5]
-- >>> riffle [1,2,3,4,5] 5 [60,70,80,90] 4
-- [1,60,70,80,2,3,4,90,5]
-- >>> riffle [1,2,3,4,5] 5 [60,70,80,90] 4
-- [60,1,2,70,3,80,4,5,90]
--
-- If the lengths are wrong, the function will sample from the wrong
-- distribution, although it will produce /an/ interleaving.
--
-- This function is part of
-- <https://apfelmus.nfshost.com/articles/random-permutations.html Heinrich Apfelmus’s “merge shuffle” algorithm>.
riffle :: MonadRandom m => [a] -> Int -> [a] -> Int -> m [a]
riffle xs     !_  []     !_  = pure xs
riffle []     !_  ys     !_  = pure ys
riffle (x:xs) !nx (y:ys) !ny = do
  k <- getRandomR (1, nx+ny)
  if k <= nx
  then (x:) <$> riffle xs     (nx-1) (y:ys) ny
  else (y:) <$> riffle (x:xs) nx     ys     (ny-1)

-- |Shuffle a list and compute its length.  Produces a permutation of the input uniformly at random.
--
-- This function uses
-- <https://apfelmus.nfshost.com/articles/random-permutations.html Heinrich Apfelmus’s “merge shuffle” algorithm>.
-- This variant of the merge shuffle traverses the list once.
--
-- >>> bridge [0..9]
-- ([6,3,7,5,2,1,9,4,8,0],10)
-- >>> bridge [0..9]
-- ([5,2,0,7,6,9,4,1,8,3],10)
bridge :: MonadRandom m => [a] -> m ([a],Int)
bridge []  = pure ([],  0)
bridge [x] = pure ([x], 1)
bridge xs  = do
  let (half1, half2) = halve xs
  (shuffled1, !length1) <- bridge half1
  (shuffled2, !length2) <- bridge half2
  shuffled              <- riffle shuffled1 length1 shuffled2 length2
  pure (shuffled, length1+length2)

-- |Shuffle a list.  Produces a permutation of the input uniformly at random.
-- 
-- This function uses
-- <https://apfelmus.nfshost.com/articles/random-permutations.html Heinrich Apfelmus’s “merge shuffle” algorithm>.
-- This variant of the merge shuffle traverses the list once.
--
-- >>> shuffle "Haskell!"
-- "elkHlas!"
-- >>> shuffle [0..9]
-- [5,6,1,7,3,9,4,2,8,0]
-- >>> shuffle [0..9]
-- [8,1,2,7,9,4,5,3,6,0]
shuffle :: MonadRandom m => [a] -> m [a]
shuffle = fmap fst . bridge
{-# INLINE shuffle #-}
