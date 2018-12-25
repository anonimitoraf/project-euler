{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import qualified Data.Set                      as S
import Utils ( isPrime )

main :: IO ()
main = print "3.hs"

largestPrimeFactor :: Int -> IO ()
largestPrimeFactor i = print largest
 where
  divideInt x = fromIntegral i / fromIntegral x
  upperBound  = round . sqrt $ fromIntegral i
  leftDivs    = [1 .. upperBound]
  rightDivs   = map divideInt leftDivs
  divPairs    = zip leftDivs rightDivs
  factorPairs = filter (\(x, y) -> isInt y) divPairs
  factors     = dedup $ mappend (map (fromIntegral . fst) factorPairs)
                                (map (fromIntegral . round . snd) factorPairs)
  largest = last $ filter isPrime factors

isInt :: RealFrac b => b -> Bool
isInt x = x == fromInteger (round x)

dedup :: (Ord a) => [a] -> [a]
dedup = S.toList . S.fromList
