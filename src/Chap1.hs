module Chap1(square) where
    square :: Float -> Float
    square x = x * x
    cmin :: (Ord a, Num a) => a -> a -> a
    cmin x y = if x < y then x else y

    quad x = square( square x)

    cmax x y = if x > y then x else y

    circle x = ( 22 / 7 ) * square x

    cthree x = 3

    smaller :: (Integer, Integer) -> Integer
    smaller (x,y) = if x <= y then x else y

    delta :: (Float, Float,Float) -> Float
    delta (a, b, c) = sqrt ( square b - ( 4 * a * c))

    smallerc :: Integer -> (Integer -> Integer)
    smallerc x y = if x <= y then x else y

    plus :: Integer -> Integer -> Integer
    plus x y = x + y

    signum :: Integer -> Integer
    signum x | x < 0 = -1
             | x ==0 = 0
             | x > 0 = 1

    cf :: (Float , Float) -> Float
    cf (x , y) = (a + 1) * (a + 2) where a = (x+y)/2

    fib :: Integer -> Integer
    fib n | n == 0 = 0
          | n == 1 = 1
          | n < 0  = error "negative number."
          | otherwise = fib (n - 2) + fib (n-1)
    cabs :: Integer -> Integer
    cabs n = if n < 0 then n * (-1) else n
