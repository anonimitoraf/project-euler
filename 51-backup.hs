-- By replacing the 1st digit of the 2-digit number *3, it turns out that six of
-- the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

-- By replacing the 3rd and 4th digits of 56**3 with the same digit, this
-- 5-digit number is the first example having seven primes among the ten
-- generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
-- 56773, and 56993. Consequently 56003, being the first member of this family,
-- is the smallest prime with this property.

-- Find the smallest prime which, by replacing part of the number (not
-- necessarily adjacent digits) with the same digit, is part of an eight prime
-- value family.

{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char ( intToDigit )
import Data.Maybe ( mapMaybe )
import Data.Foldable ( toList )
import qualified Data.Sequence as Seq
import Utils ( isPrime, replaceNths )

type InputInt = Int
type OutputInt = Int
type Positions = [Int]
type ReplacementDigit = Int
type TargetLength = Int

answer :: TargetLength -> [String]
answer t = [(show x) ++ (show ps) | x <- [1..], ps <- pss x, satisfies ps t x]
  where pss x = subsets [0..numDigits x - 1]

satisfies :: Positions -> TargetLength -> InputInt -> Bool
satisfies ps t x = length primeFamily >= t
  where rs = [0..9]
        primeFamily = filter isPrime . mapMaybe processReplacement $ rs
        processReplacement r
          | r /= 0 || (numDigits x - 1) `notElem` ps = Just (replaceWithDigit ps r x)
          | otherwise = Nothing

subsets :: [a] -> [[a]]
subsets = foldl f []
  where f b a = [a] : b ++ [ a : b' | b' <- b]

replaceWithDigit :: Positions -> ReplacementDigit -> InputInt -> OutputInt
replaceWithDigit ps d x = foldl f x ps
   where f b a = first a b + second a d + third a b
         first05 = floor
         first075 a = (10 ^^ (-a - 1))
         first1 a b = first05 (toRational b * first075 a)
         first2 a b = 10 ^ (a + 1)
         first a b = first1 a b * first2 a b
         second a d = d * 10 ^ a
         third a b = b `mod` 10 ^ a

-- replaceWithDigit ps v x = read . replaceNths ps replacements . show $ x
--   where replacements = replicate (length ps) (intToDigit v)
--         str = show v
--         z = foldl f (show x)
--           where f b a = Seq.update a (intToDigit v) (Seq.fromList b)

numDigits :: Int -> Int
numDigits n = (fromIntegral . floor . logBase 10 . fromIntegral $ n) + 1