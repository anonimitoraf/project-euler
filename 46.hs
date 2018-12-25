import Utils ( isPrime, isqrt)

main :: IO ()
main = print "46.hs"

invalidComposite :: Int
invalidComposite = fst . head . filter snd $ invalidityPairs
  where
    nums = [9..] -- start trying with 9 because we are only after odd, composite nums
    invalidityPairs = zip nums . map (\x -> odd x && isComposite x && (not . hasASolution) x) $ nums
    isComposite = not . isPrime

hasASolution :: Int -> Bool
hasASolution = not . null . solutions
  where solutions m = [ k | k <- [1 .. isqrt m], isPrime (m - 2 * k ^ 2) ]