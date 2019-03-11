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

import Data.List ( intercalate, sort )
import Data.Maybe ( mapMaybe, catMaybes )
import System.Random
import Control.Monad.State
import qualified Data.Map as Map
import Utils ( isPrime, subsets, least )

main :: IO ()
main = print "51.hs"

type Positions = [Int]
type Replacement = Int
type FamilyCount = Int
type TargetCount = Int
type SmallestPrime = Int

type RetainedDigits = String
type Substitution = (Positions, RetainedDigits)
type Result = (SmallestPrime, FamilyCount)
type Cache = Map.Map Substitution Result

answer :: TargetCount -> SmallestPrime
answer t =
  let nums = [0..]
      qqq  = checkNum t
      www = foldl (>=>) return (replicate (length nums) qqq)
  in 5

-- inMany :: Int -> KnightPos -> [KnightPos]
-- inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)
-- inMany x start = return start >>= foldl (>=>) return (replicate x moveKnight)

-- checkNumMaybe :: TargetCount -> Maybe Int State (Map.Map Substitution Result) (Maybe Int)
-- checkNumMaybe t mx = 

checkNum :: TargetCount -> Maybe Int -> State Cache (Maybe Int)
checkNum t (Just x) =  let pss = subsets [0..(calcNumDigits x - 1)]
                           ks = map (\ps -> Just $ getMapKey (ps, x)) pss
                           vs = map (\ps -> getMapValue (ps, x)) pss
                           ans = least fst $ filter (\(_, c) -> c >= t) $ catMaybes vs
                           unpackedAns = case ans of
                             Just (sp, fc) -> Just fc
                             Nothing -> Nothing
                           kvs = zip ks vs
                           unpackedKvsNoNull = unpackKvs kvs
                           updateMap m (k, v) = Map.insert k v m
                        in state $ \c -> (unpackedAns, foldl updateMap c unpackedKvsNoNull)
checkNum _ Nothing = state $ \c -> (Nothing, c)


unpackKvs :: [(Maybe Substitution, Maybe Result)] -> [(Substitution, Result)]
unpackKvs kvs = 
  let kvsNoNull = filter (\(_, v) -> 
        case v of
          Just _ -> True
          Nothing -> False
        ) kvs
      unpackedKvs = map (\(k, v) -> 
        case (k, v) of
          (Just k', Just v') -> (k', v')
        ) kvsNoNull
  in unpackedKvs

-- testMap :: Map.Map Substitution Result
-- testMap = Map.singleton ([1], "") (2, 4)

getMapKey :: (Positions, Int) -> Substitution
getMapKey (ps, x) = (ps, retained)
  where retained = extractRetained ps x

getMapValue :: (Positions, Int) -> Maybe Result
getMapValue (ps, x) = liftM2 (,) smallestPrime (return . length $ primeFamily)
  where smallestPrime = least id primeFamily
        primeFamily = filter isPrime . mapMaybe processReplacement $ [0..9]
        numDigits = calcNumDigits x
        processReplacement r
          | r /= 0 || (numDigits - 1) `notElem` ps = Just (replaceWithDigit numDigits ps r x)
          | otherwise = Nothing

extractRetained :: Positions -> Int -> String
extractRetained ps x = map snd $ filter inPs indexedCharArr
  where indexedCharArr = zip [0..] (show x)
        inPs (i, _) = i `notElem` ps

replaceWithDigit :: Int -> Positions -> Replacement -> Int -> Int 
replaceWithDigit numDigits ps d x = foldl f x psReversed
  where psReversed = map (\p -> numDigits - 1 - p) ps
        f b a = first a b + second a d + third a b
        first a b = floor (toRational b *  (10 ^^ (-a - 1))) * 10 ^ (a + 1)
        second a b = b * 10 ^ a
        third a b = b `mod` 10 ^ a

calcNumDigits :: Int -> Int
calcNumDigits n = (fromIntegral . floor . logBase 10 . fromIntegral $ n) + 1