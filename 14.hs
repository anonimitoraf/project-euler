{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.State.Strict ( State, state, runState )
import qualified Data.IntMap.Strict as M

main :: IO ()
main = print "14.hs"

f :: Int -> Int
f x = case x `mod` 2 of
      0 -> x `quot` 2
      _ -> 3 * x + 1

type Series = [Int]
cacheAndGet :: Int -> State (M.IntMap Series) Series
cacheAndGet x = state $ \cache -> let y = M.lookup x cache
                                  in  case y of
                                      Just series -> (series, cache)
                                      Nothing -> let (ys, newMap) = runState (cacheAndGet . f  $ x) cache
                                                     series = x:ys
                                                     evenNewerMap = M.insert x series newMap
                                                 in (series, evenNewerMap)

solve :: Int -> Int
solve currStart = let initialMap :: M.IntMap Series = M.singleton 1 [1]
                      f (currMaxLen, currStart, cache) a = let (collatzSeq, newMap) = runState (cacheAndGet a) cache
                                                               newMaxLen = max (length collatzSeq) currMaxLen
                                                               newStart = if newMaxLen > currMaxLen
                                                                          then a 
                                                                          else currStart
                                                           in (newMaxLen, newStart, newMap)
                      (maxLen, start, _) = foldl f (1, 1, initialMap) [1..currStart]
                  in start

-- QUESTION
-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.