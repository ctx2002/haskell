module STAL

(splitList)
where
import           DB

splitList :: Eq a => [a] -> [([a], [a])]
splitList []  = error "less than 2 elements"
splitList [x] = error "less than 2 elements"
splitList xs  = split' xs 1 (length xs)

split' :: [a] -> Int -> Int ->  [([a], [a])]
split' xs firstNth len |  firstNth < len = (take firstNth xs, drop firstNth xs) : split' xs (firstNth+1) len
                       | otherwise = []

naturals = [0..]
evens1 = [n | n <- naturals , even n]
odd1 = [n | n <- naturals , odd n]
even2 = [ 2*n | n <- naturals]
small_squares1 = [n^2 | n <- enumFromTo 0 999]

run:: Integer -> [Integer]
run n
  | n < 1 = error "argument not positive"
  | n == 1 = [1]
  | even n = n : run (div n 2)
  | odd n = n : run (3*n + 1)
