-- {-# LANGUAGE DataKinds #-}
module EssCH1 () where

import qualified Data.Tree as DT
{-- nested list, list of list --}
data List a = Elem a |  List [List a] deriving (Show, Eq)
data Symbol a = Sym a deriving (Show, Eq)

-- data Identifier String
data LcExp = Identifier String | Lambda String  LcExp | Application LcExp LcExp deriving (Show)

-- Mixed type

data MixedType = MInt Int | MString String | MInteger Integer | MChar Char deriving (Show)

data BinSearchTree a = Nil | Node a | Branch a (BinSearchTree a) (BinSearchTree a) deriving (Show)

{-- replace old with new 
 -  induction on which property? length
 - --}
subst :: (Eq a) => Symbol a -> Symbol a -> List a -> List a
subst (Sym new) (Sym old) (List [])  = List []
subst (Sym new) (Sym old) (Elem e) = if e ==  old then Elem new else Elem e
subst (Sym new) (Sym old) (List (x:xs)) = con_mylist (subst (Sym new) (Sym old) x) (subst (Sym new) (Sym old) (List xs) )


substMap :: (Eq a) => Symbol a -> Symbol a -> List (Symbol a) -> List (Symbol a)
substMap (Sym new) (Sym old) (List []) = List []
substMap (Sym new) (Sym old) (Elem (Sym subject)) = head (map (substReplace (Sym new) (Sym old)) [(Elem (Sym subject))])

substReplace :: (Eq a) => Symbol a -> Symbol a -> List (Symbol a) -> List (Symbol a)
substReplace (Sym new) (Sym old) (Elem (Sym subject)) = if subject == old then (Elem (Sym new)) else (Elem (Sym subject))

list_length :: [a] -> Integer
list_length [] = 0
list_length (x:xs) = 1 + list_length xs

mylist_length :: List a  -> Integer
mylist_length (List []) = 0
mylist_length (Elem a) = 1
mylist_length (List (x:xs)) = 1 + mylist_length (List xs)

{-- zero based --}
nth_element :: List a -> Integer -> Maybe (List a)
nth_element (List []) n = Nothing
nth_element (Elem a) n | n /= 0 = Nothing
                       | otherwise = Just (Elem a)
nth_element (List (x:xs)) n | n < 0     = Nothing
                            | n == 0    = Just x
                            | otherwise = nth_element (List xs) (n-1)

{-- remove first item from List --}
remove_first :: (Eq a) => List a -> List a -> List a
remove_first (List []) search =  List []
remove_first (Elem a) search | search == (Elem a) = List []
                             | otherwise = Elem a
remove_first (List (x:xs)) search | search /= x = con_mylist x (remove_first (List xs) search) 
                                  | otherwise = List xs


con_mylist :: List a -> List a -> List a
con_mylist (Elem a) (List []) = List [Elem a]
con_mylist (List items) (List []) = List [List items]
con_mylist (Elem a) (List items)  = List ( (Elem a) : items)
con_mylist (Elem a) (Elem b) = List [Elem a, Elem b]
con_mylist (List items) (Elem b) = List (items ++ [Elem b]) 
con_mylist (List as) (List bs) = List ([List as] ++ bs) 

occurs_free :: LcExp -> LcExp -> Bool
occurs_free (Identifier var) (Identifier s) = var == s
occurs_free (Identifier var) (Lambda id exp) = var /= id && occurs_free (Identifier var) exp
occurs_free (Identifier var) (Application exp1 exp2) = occurs_free (Identifier var) exp1 || occurs_free (Identifier var) exp2

{--
 - duple number-of-time element
 - induction on which property? on linear list property - length
 - so is induction same as  map? under which situation , we can use 
 - map to replace induction?
 - usage:
 -     input: duple 2 3
 -     output: [3,3]
 -
 -     input: duple 3 [1,2]
 -     output: [ [1,2],[1,2],[1,2] ]
 - --}

duple :: Integer -> a -> [a]
duple 0 element = []
duple 1 element = [element]
duple n element = if n < 0 then [] else element : (duple (n-1) element)

{--
 -    invert list
 -    where the list contains 2 element tuple
 -    
 -    so this function can use map instead of induction?
 -    so again, under which situation , induction can be replaced by map?
 -
 -    input: invert [(1,2),(3,4)]
 -    output: [(2,1),(4,3)]
 - --}

invert :: [(a,b)] -> [(b,a)]
invert = map (\x -> (snd x, fst x))

{--
 -    down list
 -    wraps parentheses around each top-level element of list.
 -
 -    input: down [ (Elem 1)]
 -    output: [List [(Elem 1)]]
 -
 -    input: down [(Elem 1), (Elem 2)]
 -    output: [ List [(Elem 1)], List [(Elem 2)] ]
 -
 -    input: down [ List []]
 -    output: [List [List []]]
 - --}

down :: [List a] -> [List a]
down = map (\x -> List [x])

{--
 - swapper a b list
 - all occurences of a replace by b and all b(s) replaced by a
 -
 - swapper (Sym 1) (Sym 2) [(Elem (Sym 1))]
 - [(Elem (Sym 2))]
 -
 - swapper (Sym 1) (Sym 2) [ (Elem (Sym 1)), (Elem (Sym 2)) ]
 - [Elem (Sym 2),Elem (Sym 1),Elem (Sym 1)]
 -
 - swapper (Sym 1) (Sym 2) [ (Elem (Sym 1)), (Elem (Sym 2)), (Elem (Sym 2)),List [(Elem (Sym 2)), (Elem (Sym 1))] ]
 - [Elem (Sym 2),Elem (Sym 1),Elem (Sym 1),List [Elem (Sym 1),Elem (Sym 2)]]
 - --}
swapper :: (Eq a) => Symbol a -> Symbol a -> [List (Symbol a) ] -> [List (Symbol a) ]
swapper one two  = map (\x -> swapper' one two x)

swapper' :: (Eq a) => Symbol a -> Symbol a -> List (Symbol a) -> List (Symbol a)
swapper' (Sym new) (Sym old) (List [])  = List []
swapper' (Sym new) (Sym old) (Elem (Sym e)) = if e ==  old then Elem (Sym new) else if e == new then Elem (Sym old) else Elem (Sym e)
swapper' (Sym new) (Sym old) (List (x:xs)) = con_mylist (swapper' (Sym new) (Sym old) x) (swapper' (Sym new) (Sym old) (List xs) )

{--
 -   list is 0 based
 -
 -   listSet list position element
 -   input:  listSet  [(Elem (Sym 1)), (Elem (Sym 2))] 0 (Elem (Sym 3))
 -   output: [Elem (Sym 3),Elem (Sym 2)]
 -
 - --}

listSet :: [List a] -> Integer -> List a -> [List a]
listSet = listSetOther

listSetOther :: [List a] -> Integer -> List a -> [List a]
listSetOther [] n replacement = []
listSetOther (x:xs) 0 replacement = replacement: xs
listSetOther (x:xs) n replacement = x: listSetOther xs (n-1) replacement

listSplit :: [List a] -> Integer -> [List a]
listSplit [] position = []

{--
 -   count occureences of an element
 -   count_occurrences element list
 -   input:  count_occurrence (Elem (Sym 1)) [(Elem (Sym 1)), List [(Elem (Sym 1)), (Elem (Sym 2))]]
 -   output: 2
 - --}
count_occurrences :: (Eq a) => List a -> [List a] -> Integer
count_occurrences (Elem a) [] = 0
count_occurrences (Elem a) [List []] = 0
count_occurrences (Elem a) [Elem b] = if a == b then 1 else 0
count_occurrences (Elem a) [List (x:xs)] = count_occurrences (Elem a) [x] + count_occurrences (Elem a) xs
count_occurrences (Elem a) (x:xs) = count_occurrences (Elem a) [x] + count_occurrences (Elem a) xs

{--
 -    my_product list list 
 -    input: product [1,2] [3,4]
 -    output: [ (1,3), (1,4), (2,3), (2,4) ]
 -
 -    look at this code, in Haskell , it has a special name call zip
 -
 -    inductionable structure
 - --}
myProduct :: [a] -> [b] -> [(a,b)]
myProduct xs [] = [] 
myProduct [] ys = []
myProduct (x:xs) yss = myProduct' x yss ++ myProduct xs yss 

{--
 - we can use map now, looks like any form as below can be use map:
 - 1 form is empty
 - 2 the immediatly use (x:xs) form, do something on x , then induct on xs
 - --}
myProduct' :: a -> [b] -> [(a,b)]
myProduct' element = map (\x -> (element,x))


{--
 -    filter_in pred list
 - --}
filter_in :: (a -> Bool) -> [a] -> [a]
filter_in f [] = []
filter_in f (x:xs) = 
    let 
        y = f x
    in
       if y then x : filter_in f xs else filter_in f xs

other_filter_in :: (a -> Bool) -> [a] -> [a] -> [a]
other_filter_in f [] accum = accum
other_filter_in f (x:xs) accum = 
    let 
        y = f x
    in
        let z = if y then accum ++ [x] else accum
        in other_filter_in f xs z

{--
 -  (list-index pred lst) returns the 0-based position of the
 -  first element of lst that satisfies the predicate pred.
 - --}

list_index :: (a -> Bool) -> [a] -> Maybe Integer
list_index f [] = Nothing
list_index f xs = 
    let p = list_index' f xs (toInteger (length xs)) 0
    in
        if p == (-1) then Nothing else Just p

list_index' :: (a -> Bool) -> [a] -> Integer -> Integer -> Integer
list_index' f [] length position = (-1)
list_index' f (x:xs) length position = if f x && position < length then position else if position >= length then (-1) else list_index' f xs length (position+1)

list_index_other :: (a -> Bool) -> [a] -> Maybe Integer
list_index_other f xs  = list_index_other' f xs 0 

list_index_other':: (a -> Bool) -> [a] -> Integer -> Maybe Integer
list_index_other' f [] position     = Nothing
list_index_other' f (x:xs) position = if f x then Just position else list_index_other' f xs (position+1)

{--
 - (every? pred lst) returns #f if any element of lst fails to
 - satisfy pred, and returns #t otherwise
 - --}

my_every :: (a -> Bool) -> [a] -> Bool
my_every f [] = True
-- my_every f (x:xs) = if f x then my_every f xs else False
my_every f (x:xs) = f x && my_every f xs

my_exists :: (a -> Bool) -> [a] -> Bool
my_exists f [] = False
my_exists f (x:xs) = f x || my_exists f xs

{--
 -  interesting observation
 -  if we only want do something on top level, then do nothing on head (see
 -  my_up, ys no been processed.)
 -
 -  otherwise, when we want to do something on every element, we should
 -  recusively doing it on head (see my_flatten, ys recusiviely processed)
 - --}
my_up :: [List a] -> [List a]
my_up [] = []
my_up (x:xs) = case x of (Elem e) -> (Elem e) : my_up xs
                         (List ys) -> ys ++ my_up xs

my_flatten :: [List a] -> [List a]
my_flatten [] = []
my_flatten (x:xs) = case x of (Elem e) -> (Elem e) : my_flatten xs
                              (List ys) -> my_flatten ys ++ my_flatten xs

{--
 -
 - --}

merge :: (Ord a) => [List a] -> [List a] -> [List a]
merge [] ys = ys
merge xs [] = xs
merge ((Elem x):xs) ((Elem y):ys) = 
    if x == y then (Elem x):(Elem y): merge xs ys 
    else if x < y then (Elem x): merge xs ((Elem y):ys) else (Elem y): merge ((Elem x):xs) ys

my_splitAt :: [List a] -> Integer -> ([List a], [List a])
my_splitAt  xs position = if position < 0 then (xs, [])
    else my_splitAt' xs position []

my_splitAt' :: [List a] -> Integer -> [List a] -> ([List a], [List a])
my_splitAt' [] position accu = (accu, [])
my_splitAt' (x:xs) 0 accu    = (x:accu, xs)
my_splitAt' (x:xs) n accu    = my_splitAt' xs (n-1) (x:accu)

my_merge_sort :: (Ord a) => [List a] -> [List a]
my_merge_sort []  = []
my_merge_sort [x] = [x]
my_merge_sort xs  = let li = my_splitAt xs (toInteger (length xs `mod` 2))
                    in
                        merge (my_merge_sort (fst li)) (my_merge_sort (snd li))

{--
 - find first occurrce in binary search tree
 - --}
my_path :: Integer -> BinSearchTree Integer -> [String]
my_path n Nil = []
my_path n tree = my_path' n tree []

my_path' :: Integer -> BinSearchTree Integer -> [String] -> [String]
my_path' n (Node item) accu = if n == item then accu else []
my_path' n (Branch item left right) accu = if n == item then accu else
    let ls = (my_path' n left (accu ++ ["left"])) 
    in
       if ls == [] then (my_path' n right (accu ++ ["right"])) else ls
{--
    Write a procedure number-leaves that takes a bintree, and
    produces a bintree like the original, except the contents of the leaves are numbered
    starting from 0.
	
	(number-leaves
		(interior-node ’foo
		(interior-node ’bar
			(leaf 26)
			(leaf 12))
		(interior-node ’baz
			(leaf 11)
		(interior-node ’quux
			(leaf 117)
			(leaf 14))
	should returns
	
	(foo
		(bar 0 1)
	(baz
		2
	(quux 3 4)))
--}
number_leaves ::  BinSearchTree a -> BinSearchTree Integer
number_leaves Nil = Nil
number_leaves tree = fst (number_leaves' tree 0)

number_leaves' :: BinSearchTree a -> Integer ->  (BinSearchTree Integer, Integer)
number_leaves' (Node c) count = (Node (count+1), (count+1))
number_leaves' (Branch item left right) count = 
    case left of 
        (Node item) -> let t = number_leaves' left count
                    in  
                        let rt = number_leaves' right (snd t)
                        in
                            (Branch (count+1) (fst t) (fst rt), (snd rt))
        _        -> let lt = number_leaves' left count
                    in
                        let rt = number_leaves' right (snd lt)
                        in
                            (Branch (count+1) (fst lt) (fst rt), (snd rt))

                        
