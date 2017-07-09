module Chap1(square) where
    square x = x * x

    cmin x y = if x < y then x else y

    data SList a = SList a [SList a] deriving Show
    data MarkTree a = Leaf a | InterNode String (MarkTree a) (MarkTree a) deriving (Show , Ord, Eq)

    -- return a list contains n copy of x
    duple :: Integer -> a -> [a]
    duple 0 x = []
    duple n x = x : duple (n-1) x

    -- (invert lst), where lst is a list of 2-lists (lists of length two),
    -- returns a list with each 2-list reversed.
    invert :: [[a]] -> [[a]]
    invert ([a,b]:xs) = [b,a] : invert xs
    invert []         = []

    numberLeaves ::  MarkTree Integer -> MarkTree Integer
    numberLeaves  = numberLeavesZ 0


    numberLeavesZ ::  Integer -> MarkTree Integer -> MarkTree Integer
    numberLeavesZ entry (Leaf x) = Leaf entry
    numberLeavesZ entry (InterNode str (Leaf x) right) = InterNode str (Leaf entry) (numberLeavesZ (entry+1) right)
    numberLeavesZ entry (InterNode str left (Leaf x) ) = InterNode str (numberLeavesZ (entry+1) left) (Leaf entry )
    numberLeavesZ entry (InterNode str left right ) = InterNode str (numberLeavesZ entry left) (numberLeavesZ (entry+2) right )
    --numberLeavesZ entry (InterNode str (Leaf y) (Leaf x) ) = InterNode str (Leaf entry) (Leaf (entry+1) )
    --numberLeavesZ entry (Leaf x) exist = Leaf entry
    --numberLeavesZ entry (InterNode str left (Leaf x)) exist = InterNode str (numberLeavesZ entry left (entry+1)) (Leaf exist)
    --numberLeavesZ entry (InterNode str (Leaf x) right) exist =
    --    InterNode str (Leaf entry) (numberLeavesZ exist right (exist+1))
    --numberLeavesz n (InterNode str (Leaf x) (Leaf y)) = InterNode str (Leaf n) (Leaf (n+1))
    --numberLeavesZ n (InterNode str left right) = InterNode str (numberLeavesZ n left )  (numberLeavesZ (n+1) right)
    --numberLeavesZ n (Leaf a ) = Leaf n

    units, teens, tens :: [String]
    units = ["zero","one","two","three","four","five", "six","seven","eight","nine"]
    teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen", "nineteen"]
    tens =  ["twenty","thirty","forty","fifty","sixty", "seventy","eighty","ninety"]

    converts1 :: Int -> String
    converts1 n = units !! n

    digital2 :: Int -> (Int, Int)
    digital2 n = (div n 10, mod n 10)

    -- 0 <= n < 100
    combine2 :: (Int,Int) -> String
    combine2 (t, u) | t == 0 = units !! u
        | t == 1 = teens !! u
        | 2 <= t && u == 0 = tens !! (t-2)
        | 2 <= t && u /= 0 = tens !! (t-2) ++ "-" ++ units !! u

    convert2 :: Int -> String
    convert2 = combine2 . digital2

    myMen :: Int -> String
    myMen n | n == 1 = units !! n ++ " man"
            | otherwise = units !! n ++ " men," ++ myMen (n-1)

