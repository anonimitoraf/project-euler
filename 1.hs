sumMult3And5 :: Int -> Int
sumMult3And5 n = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..(n-1)] 