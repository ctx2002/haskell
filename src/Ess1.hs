module Ess1 where

    

    isN :: Integer -> Bool
    isN n | n == 0 = True
           | n < 0 = False
           | otherwise = isN (n-3)

    nthEle :: Integer -> [a] -> a
    nthEle n [] = error "empty list"
    nthEle n (x:xs)
        | n == 0 = x
        | n < 0 = nthEle (n*(-1) - 1) (reverse (x:xs))
        | otherwise = nthEle (n-1) xs

    nthEle1 :: Integer -> [a] -> a
    nthEle1 n li = case (n,li) of
        (0,x:xs)  -> x
        (_, [])   -> error "empty list"
        (n, x:xs) -> if n < 0 then nthEle1 (n*(-1) - 1) (reverse (x:xs)) else nthEle1 (n-1) xs


    nthEle2 :: Integer -> [a] -> Maybe a
    nthEle2 _ []     = Nothing
    nthEle2 0 (x:xs) = Just x
    nthEle2 n (x:xs) = nthEle2 (n-1) xs

    myLength :: [a] -> Integer
    myLength  = foldr (\x -> (+) 1) 0

    myLength1 :: [a] -> Integer
    myLength1 []     = 0
    myLength1 (x:xs) = (+1) (myLength1 xs)

    myLength2 :: [a] -> Integer
    myLength2 = sum . map (const 1)

    removeFirst :: (Eq a) => a -> [a] ->[a] -> [a]
    removeFirst _ [] rs = reverse rs
    removeFirst k (x:xs) rs = if k == x then reverse rs ++ xs else removeFirst k xs (x:rs)

    removeFirst1 :: (Eq a) => a -> [a] -> [a]
    removeFirst1 _ [] = []
    removeFirst1 k (x:xs) | k == x = xs
                          | otherwise = x :  removeFirst1 k xs

    removeFirstAll :: (Eq a) => a -> [a] -> [a]
    removeFirstAll a xs = [x | x <- xs, x /= a]

    removeFirstWrap :: (Eq a) => a -> [a] -> [a]
    removeFirstWrap x y = removeFirst x y []

    {-
        mysplitAt list position
        input: mysplitAt [] 0
        output: ([],[])

        input: mysplitAt [1] 0
        output: ([1], [])

        input mysplitAt[1] 2
        output: ([1], [])

        input: mysplitAt [1,2,3] -1
        output: ([1,2,3], [])

        input: mysplitAt [1,2,3] 0
        output: ([1],[2,3])
    -}

    mysplitAt :: [a] -> Integer -> ([a], [a])
    mysplitAt [] n = ([],[])
    mysplitAt (x:xs) n = if n < 0 then (x:xs, []) else mysplitAt' (x:xs) n []

    mysplitAt' :: [a] -> Integer -> [a] -> ([a], [a])
    mysplitAt' [] n accu = (accu, [])
    mysplitAt' (x:xs) n accu = if n == 0 then (accu++[x], xs) else mysplitAt' xs (n-1) (x:accu)


