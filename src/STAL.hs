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
