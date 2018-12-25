module Utils ( isPrime, isqrt, replaceNth, replaceNths ) where

isPrime :: Int -> Bool
isPrime k = k >= 2 && null [ x | x <- [2 .. isqrt k], k `mod` x == 0 ]

isqrt :: Int -> Int
isqrt x = round . sqrt $ fromIntegral x

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

replaceNths :: [Int] -> [a] -> [a] -> [a]
replaceNths _ _ [] = []
replaceNths [] _ xs = xs
replaceNths _ [] xs = xs
replaceNths (n:ns) (v:vs) xs = replaceNths ns vs (replaceNth n v xs)