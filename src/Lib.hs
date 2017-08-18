module Lib
    (someFunc 
    ) where
    
import Data.Char
import qualified Data.Text as T
import Data.Tree
import Data.Maybe

someFunc :: IO()
someFunc = putStrLn "hello, world"
--type Identifier a = a
data BigNum a = BigEmpty | BigCon a (BigNum a) deriving (Show)
data LcExp = Identifier String | Lambda String (LcExp) | Application LcExp LcExp
data List a = Empty | Con a (List a) deriving Show 
data SList a = SList a [SList a] deriving Show 
data BinTree a = NodeEmpty | TreeNode a (BinTree a) (BinTree a) deriving (Eq,Ord,Show,Read)
data MarkTree a = Leaf a   | InteriorNode String (MarkTree a) (MarkTree a) deriving (Eq, Ord,Show,Read)
--data SEList a = TEmpty | SExp a deriving Show
--data SExp a   = Symbol a | SEList [SExp a] deriving Show
is_n :: Integer -> Bool
is_n x | x < 0 = False
       | x == 0 = True
       | otherwise = is_n (x - 3)

my_list :: [a] -> Int
my_list [] = 0
my_list (_:xs) = 1 + (my_list xs)

my_length :: [a] -> Int
my_length [] = 0
my_length xs = fst $ last (zip [1..] xs)

cal_length :: [a] -> Int
cal_length xs = cal_length_z xs 0
    where cal_length_z [] len = len
          cal_length_z xs len = cal_length_z (tail(xs)) len+1 

--my_nthele :: String -> Integer -> Char
--my_nthele [] n = ''
--my_nthele (x:xs) n  | n == 0 = x
--                    | otherwise = my_nthele xs (n-1)

nthele :: Integer -> [a] -> a
nthele n [] = error "list is empty"
nthele n (x:xs) | n == 0 = x
                | n < 0 = nthele (n*(-1) - 1) (reverse (x:xs)) 
                | otherwise = nthele (n-1) xs

ntheledup :: Show a => Integer -> [a] -> a
ntheledup n xs = ntheledup_z n xs xs n
    where ntheledup_z n orgi [] c = error ( (show orgi) ++ " does not have  "++ (show (howmanyele c))++" elements.")
          ntheledup_z n orgi (x:xs) c | n == 0 = x
              | n < 0 = ntheledup_z (n*(-1) - 1) orgi (reverse (x:xs)) c
              | otherwise = ntheledup_z (n-1) orgi xs c

howmanyele :: Integer -> Integer
howmanyele n | n < 0 = n*(-1)
             | n == 0 = 0
             | otherwise = n+1
           

nthelemaybe :: Integer ->[a] -> Maybe a
nthelemaybe n [] = Nothing
nthelemaybe n (x:xs) | n == 0 = Just x
    | n < 0 = nthelemaybe (n*(-1) - 1) (reverse (x:xs))
    | otherwise = nthelemaybe (n-1) xs


nthelecase :: Integer -> [a] -> a
nthelecase n li = case (n,li) of
    (0, x:_) -> x
    (_, [])  -> error "empty"
    (k, x:xs) -> if k<0 then nthelecase (k+1) (x:xs) else  nthelecase (k-1) xs

butOne :: [a] -> Maybe a
butOne []   = Nothing
butOne (x:[]) = Nothing
butOne (x:y:[]) = Just x
butOne (x:y:xs) = butOne (y:xs)

mySub ::  Eq a => a -> a -> SList a -> SList a
mySub new old (SList x []) = if x == old then SList new [] else SList x []
mySub new old (SList x (y:ys)) = 
    if x == old then SList new ( mySub_z new old (y:ys)) 
                else SList x ( mySub_z new old (y:ys)) 

mySub_z :: Eq a => a -> a -> [SList a] -> [SList a]
mySub_z new old [] = []
mySub_z new old (x:xs) = (mySub new old x) : (mySub_z new old xs)



myRev :: [a] -> [a]
myRev str = go [] str
    where
        go rev [] = rev
        go rev (x:xs) = go (x:rev) xs

occursFree :: String -> LcExp -> Bool
occursFree  x (Identifier y)   = if (x == y) then True else False
occursFree  x (Lambda y (e)) = if (x == y) then False else occursFree x e
occursFree  x (Application lamb1 lamb2) = occursFree x lamb1 || occursFree x lamb2

myIndex :: Num t => [a] -> t -> [(t, a)]
myIndex [] n = []
myIndex (x:xs) n = myIndex_z ((n,x):[]) xs (n+1) 
    where myIndex_z acc [] n = acc
          myIndex_z acc (x:xs) n = myIndex_z (acc ++ [(n,x)]) xs (n+1)

myDup :: a -> Integer ->[a]
myDup x 0 = []
myDup x n = x: (myDup x (n-1)) 

myInvert :: [[a]] -> [[a]]
myInvert [] = []
myInvert ([a,b]:xs) = [b,a] : myInvert xs

myWrap :: [a] -> [[a]]
myWrap [] = []
myWrap (x:xs) = [x] :  myWrap xs

swapper :: Eq a => a -> a -> [a] -> [a]
swapper x y [] = []
swapper x y [z] | x == z = [y]
                | y == z = [x]
                | otherwise = [z] 
swapper x y (s:sm) = ( swapper x y [s] ) ++ (swapper x y sm) 

count_occurrences :: Eq a => a -> [a] -> Integer
count_occurrences x (y:ys) = count_occurrences_z x (y:ys) 0
    where 
        count_occurrences_z x [] n = n
        count_occurrences_z x (y:ys) n | x == y = count_occurrences_z x ys (n+1)
                                       | otherwise = count_occurrences_z x ys n

doubleTree :: Num a => BinTree a -> BinTree a
doubleTree NodeEmpty = NodeEmpty
doubleTree (TreeNode n NodeEmpty NodeEmpty) = TreeNode (2*n) NodeEmpty NodeEmpty
doubleTree (TreeNode n t1 t2) = TreeNode (2*n) (doubleTree t1) (doubleTree t2)

cproduct :: [a] -> [a] -> [[a]]
cproduct [] [] = []
cproduct [] (x:xs) = []
cproduct (x:xs) [] = []
cproduct (x:xs) (y:ys) = cproduct_z x (y:ys) ++  cproduct xs (y:ys)

cproduct_z :: a -> [a] -> [[a]]
cproduct_z x [] = []
cproduct_z x (y:ys) = [x,y] :  cproduct_z x ys

markMyTree :: Integral a => MarkTree a -> MarkTree a
markMyTree (Leaf x) = Leaf 0
markMyTree (InteriorNode str (lft) (rgt)) | str == "red" = (InteriorNode str (markMyTree_z 1 (lft)) (markMyTree_z 1 (rgt)  ))
           | otherwise = InteriorNode str (markMyTree_z 0 (lft)) (markMyTree_z 0 (rgt)  )

markMyTree_z :: Integral a =>  a -> MarkTree a -> MarkTree a
markMyTree_z n (Leaf x) = Leaf n
markMyTree_z n (InteriorNode str (lft) (rgt)) | str == "red" = InteriorNode str (markMyTree_z (1+n) (lft))  (markMyTree_z (1+n) (rgt))
    | otherwise = InteriorNode str (markMyTree_z (n) (lft))  (markMyTree_z (n) (rgt))

       
myEvery :: (a->Bool) -> [a] -> Bool
myEvery  pred [] = True
myEvery  pred (x:xs) = (pred x) && myEvery pred xs

myExist :: (a -> Bool) -> [a] -> Bool
myExist pred [] = False
myExist pred (x:xs) | (pred x) = True
    | otherwise = myExist pred xs

-- merge sorted 2 lists
myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] xs = xs
myMerge xs [] = xs
myMerge first@(x:xs) second@(y:ys)
    | x <= y = x : (myMerge xs second)
    | otherwise = y: (myMerge first ys)

myQuick :: Ord a => [a] -> [a]
myQuick [] = []
myQuick (x:xs) = myQuick small ++ (x: myQuick large)
    where small = [y | y <- xs, y<= x]
          large = [y | y <- xs, y > x]

bigNumber :: Integral a => a -> a -> BigNum a
bigNumber 0 base = BigEmpty 
bigNumber a base = 
    if a < 0 then error "no negative number."  else
    let r = mod a base
        q = div a base
    in
        BigCon r (bigNumber q base) 

bigZero :: BigNum a
bigZero = BigEmpty

isBigZero :: BigNum a -> Bool
isBigZero BigEmpty = True
isBigZero (BigCon a x) = False

big2Normal ::  (BigNum Integer) -> Integer -> Integer
--big2Normal BigEmpty base = 0
big2Normal body base = big2NormalZ body base 0
    where  big2NormalZ  BigEmpty base n = 0 
           big2NormalZ (BigCon r rest) base n = (r * (base ^ n)) + big2NormalZ rest base (n+1)

bigPre :: (BigNum Integer) -> Integer -> (BigNum Integer)
bigPre body base = if isBigZero body then error "zero has no presuccessor" else 
    let 
        i = ((big2Normal body base) - 1)
    in  
        bigNumber i base 

bigSucc :: (BigNum Integer) -> Integer -> (BigNum Integer)
bigSucc body base = let 
    i = ((big2Normal body base) + 1)
  in
    bigNumber i base 



big2List :: BigNum a -> [a]
big2List BigEmpty = []
big2List (BigCon r q) = r : big2List q    
--myWrapSingle :: a -> [a]
--myWrapSingle x = [x]

modernise :: String -> String
modernise [] = []
modernise (x:str) = let part = span isAlpha (x:str)
    in
        if x == ' ' then x:modernise str else
        ((toUpper (head $ fst part) ): (tail $ fst part)) ++ (modernise $ snd part)

-- how to use Text package.
moderniseT :: T.Text -> T.Text
moderniseT tx = T.unwords (  moderniseT' (T.split (==' ') tx) )

moderniseT' :: [T.Text] ->[T.Text]
moderniseT' [] = []
moderniseT' (x:xs) =
    --let myVal = T.uncons x
    let (firstChar, restText) = Data.Maybe.fromMaybe (' ', (T.pack "")) (T.uncons x)
    in
        (T.cons (toUpper firstChar) restText): (moderniseT' xs)

