{-# LANGUAGE MultiWayIf #-}

module Helpers_54 (
    evalInts
  , evalMaybeInts
  , evalMaybeIntTups
  , evalBools
  , getGrp
  , sameSuit
  , eqNum
  , cmpNum
  , highestVal
  , isSequential
  , removeFirstNBy
  , chunksOf
) where

import           Data.Ord                       ( comparing )
import           Data.List                      ( sort
                                                , sortBy
                                                , maximumBy
                                                , groupBy
                                                , partition
                                                )
import           Typedefs_54

-- TODO: These eval functions are ugly and non-scalable. Think of ways to do better

evalBools :: Bool -> Bool -> Maybe Winner
evalBools False False = Nothing
evalBools True  True  = Nothing
evalBools True  False = Just Player1
evalBools False True  = Just Player2

evalInts :: Int -> Int -> Maybe Winner
evalInts a b | a == b     = Nothing
             | a > b      = Just Player1
             | otherwise  = Just Player2

evalMaybeInts :: Maybe Int -> Maybe Int -> Maybe Winner
evalMaybeInts Nothing Nothing = Nothing
evalMaybeInts x Nothing = Just Player1
evalMaybeInts Nothing x = Just Player2
evalMaybeInts (Just i) (Just j) = evalInts i j

evalMaybeIntTups :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe Winner
evalMaybeIntTups Nothing Nothing = Nothing
evalMaybeIntTups x Nothing = Just Player1
evalMaybeIntTups Nothing x = Just Player2
evalMaybeIntTups (Just (a, a2)) 
                 (Just (b, b2)) = if | a > b -> Just Player1
                                     | a < b -> Just Player2
                                     | otherwise -> if | a2 > b2 -> Just Player1
                                                       | a2 < b2 -> Just Player2
                                                       | otherwise -> Nothing

getGrp :: Int -> Int -> [Card] -> Maybe Int
getGrp occ sz cs =
  let revSortedCs = sortBy (flip cmpNum) cs
      groupedByNum    = groupBy eqNum revSortedCs
      -- validGrps is sorted desc (due to revSortedCs being sorted already)
      validGrps = filter (\g -> length g >= sz) groupedByNum
  in  if length validGrps < occ
        then Nothing
        else
          let highestGrpNum =
                if (null validGrps) || (null $ head validGrps)
                  then Nothing
                  else Just (fst . head . head $ validGrps)
          in  highestGrpNum

sameSuit :: [Card] -> Bool
sameSuit [] = True
sameSuit cs = all cmpSuit cs where cmpSuit c = snd c == snd (head cs)

eqNum :: Card -> Card -> Bool
eqNum a b = fst a == fst b

cmpNum :: Card -> Card -> Ordering
cmpNum = comparing fst

highestVal :: [Card] -> Int
highestVal = fst . maximumBy cmpNum

isSequential :: [Int] -> Bool
isSequential [] = True
isSequential xs = let minNum = minimum xs
                      indexedNums = zip [0..] $ sort xs
                  in all (\x -> snd x - fst x == minNum) indexedNums

removeFirstNBy :: (a -> Bool) -> Int -> [a] -> [a]
removeFirstNBy _ 0 xs = xs
removeFirstNBy f n xs = let (validYs, invalidYs) = partition f xs
                            remainingYs = drop n validYs
                        in remainingYs ++ invalidYs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)