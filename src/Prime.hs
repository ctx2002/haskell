module Prime(
    divides,prime0,mnmInt,removeFst,sortL,
    blowup,prefix,lastE,myInit,lastS,myLength,sumLength
)
 where

divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

ld :: Integral a => a -> a
ld   = ldf 2

prime0 n | n < 1 = error "not a pisitive integer"
prime0 n | n == 1 = False
prime0 n = ld n == n

ldf k n | divides k n = k
ldf k n | k^2 > n = n
ldf k n = ldf (k+1) n

mnmInt :: [Int] -> Int
mnmInt []     = error "empty list"
mnmInt [x]    = x
mnmInt (x:xs) = min x (mnmInt xs)

removeFst :: Eq t => t->[t]->[t]
removeFst _ [] = []
removeFst x (b:bs)
 | x == b = bs
 | otherwise = b : removeFst x bs

sortL :: [Int] -> [Int]
sortL [] = []
sortL xs = m : sortL(removeFst m xs) where m = mnmInt xs

countChar []     = 0
countChar [x]    = 1
countChar (x:xs) = 1 + countChar xs

blowup :: String -> String
blowup []     = []
blowup (c:cs) = blowup' 1 (c:cs)
blowup' :: Int -> String -> String
blowup' i []     = []
blowup' i (c:cs) = replicate i c ++ blowup' (i+1) cs

prefix :: String -> String -> Bool
prefix [] ys         = True
prefix xs []         = False
prefix (x:xs) (y:ys)
 | x == y = prefix xs ys
 | otherwise = False

bblowuup :: String -> Int -> String
bblowuup "" start     = ""
bblowuup (x:xs) start = replicate start x ++ bblowuup xs (start+1)

lastE :: [a] -> a
lastE []     = error "No end for empty lists!"
lastE [x]    = x
lastE (_:xs) = lastE xs

myInit :: [a] -> [a]
myInit []     = error "empty list"
myInit [_]    = []
myInit (x:xs) = x: myInit xs



lastS :: [a] -> a
lastS []  = error "No second last element for empty list!"
lastS [_]=error "No second last element for one element list!"
lastS xs  = last (init xs)


substr :: String -> String -> Bool
substr [] [] = True
substr (_:_) [] = False
substr [] (_:_) = True
substr (x:xs) (y:ys)
 | x == y && prefix xs ys = True
 | otherwise = substr (x:xs) ys

myLength :: [[a]] -> [Int]
myLength [] = [0]
myLength xs = map length xs

sumLength :: [[a]] -> Int
sumLength [] = 0
sumLength xs = sum (map length xs)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse [z]    = [z]
myReverse (x:xs) = myReverse xs ++ [x]
