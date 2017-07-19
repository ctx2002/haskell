module Chap2 (

    ) where


-- specification
-- we use haskell's number presentation

zero :: Integer -> Integer
zero n = 0

isZero :: Integer -> Bool
isZero 0 = True
isZero n = False

successor :: Integer -> Integer
successor n = n + 1

predecessor :: Integer -> Integer
predecessor n = n -1

myadd :: Integer -> Integer -> Integer
myadd 0 y = y
myadd x y = successor ( myadd (predecessor x) y)

--Bignum representation: In the bignum representation, numbers are represented
--in base N, for some large integer N. The representation becomes
--a list consisting of numbers between 0 and N −1 (sometimes called bigits
--rather than digits
-- define n = () when n = 0
--        n = (r . q ) when n = qN + r (0 <= r < N)

bigit :: Integer -> Integer -> [Integer]
bigit 0 base = [0]
bigit n base =
    let q = div n base
        r = mod n base
    in
       if r == 0 then [0, q] else r: bigit q base
ddouble :: Integer -> Integer
ddouble n = n * 2



