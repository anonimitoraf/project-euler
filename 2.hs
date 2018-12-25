sumEvenFib :: Int -> Int
sumEvenFib n = sum evenFibs
 where
  fibs     = map fibHelper [1 .. n]
  evenFibs = filter even fibs

fibHelper :: Int -> Int
-- fib = (map fib' [0 ..] !!)
--  where
--   fib' 0 = 1
--   fib' 1 = 1
--   fib' n = fib (n - 2) + fib (n - 1)
fibHelper i = fib 1 1 !! i

fib :: Int -> Int -> [Int]
fib a b = a : fib b (a + b)

minFib :: Int -> Int
minFib = minFibHelper 0

minFibHelper :: Int -> Int -> Int
minFibHelper n t =
  if fibHelper n >= t
    then n
  else minFibHelper (n+1) t

main :: IO ()
main = print "2.hs"