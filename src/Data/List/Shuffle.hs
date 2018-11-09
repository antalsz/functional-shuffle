{-# LANGUAGE BangPatterns #-}

module Data.List.Shuffle (
  halve,
  riffle,
  bridge,
  shuffle
) where

import Control.Monad.Random

halve :: [a] -> ([a],[a])
halve []     = ([],[])
halve (z:zs) = let (xs,ys) = halve zs
               in (z:ys, xs)

riffle :: MonadRandom m => [a] -> Int -> [a] -> Int -> m [a]
riffle xs     !_  []     !_  = pure xs
riffle []     !_  ys     !_  = pure ys
riffle (x:xs) !nx (y:ys) !ny = do
  k <- getRandomR (1, nx+ny)
  if k <= nx
  then (x:) <$> riffle xs     (nx-1) (y:ys) ny
  else (y:) <$> riffle (x:xs) nx     ys     (ny-1)

bridge :: MonadRandom m => [a] -> m ([a],Int)
bridge []  = pure ([],  0)
bridge [x] = pure ([x], 1)
bridge xs  = do
  let (half1, half2) = halve xs
  (shuffled1, !length1) <- bridge half1
  (shuffled2, !length2) <- bridge half2
  shuffled              <- riffle shuffled1 length1 shuffled2 length2
  pure (shuffled, length1+length2)

shuffle :: MonadRandom m => [a] -> m [a]
shuffle = fmap fst . bridge
