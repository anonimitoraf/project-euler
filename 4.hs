-- TODO: Improve this solution

import Data.List ( sortBy )

main :: IO ()
main = print "4.hs"

largestPalProduct :: [Integer]
-- largestPalProduct = 4
largestPalProduct = map fst . take 10 .filter snd $ palPairs
  where
    nums = [999, 998..100]
    products = sortBy (flip compare) $ (*) <$> nums <*> nums
    -- isPals = map (\_ -> True) products
    isPals = map (isPalindrome . fromIntegral) products
    palPairs = zip products isPals

isPalindrome :: Integer -> Bool
isPalindrome num = and cmps
 where
  cmps = zipWith (==) numStr reverseNumStr
  numStr = show num
  reverseNumStr = reverse numStr