-- {-# LANGUAGE DataKinds #-}
module EssCH1 () where

import qualified Data.Tree as DT
{-- nested list, list of list --}
data List a = Elem a |  List [List a] deriving (Show, Eq)
data Symbol a = Sym a deriving (Show, Eq)

-- data Identifier String
data LcExp = Identifier String | Lambda String  LcExp | Application LcExp LcExp deriving (Show)

{-- replace old with new --}
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
list_length (x:xs) = 1 + list_length(xs)

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
 -   listSet list position element
 - --}

listSet :: [List a] -> Integer -> List a -> [List a]
listSet xs  position replacement | position < 0 = xs
                                 | otherwise = listSetCheck [] xs replacement position 0

listSetCheck :: [List a] -> [List a] -> List a -> Integer -> Integer -> [List a]
listSetCheck first (x:xs) replacement end start = if end == start then first ++ [replacement] ++ xs 
                                                  else if start > end then first ++ (x:xs) else  listSetCheck (first ++ [x]) xs replacement end (start + 1)

listSetOther :: [List a] -> Integer -> List a -> [List a]
listSetOther [] position replacement = []
listSetOther (x:xs) 0 replacement = replacement: xs
listSetOther (x:xs) n replacement = x: listSetOther xs (n-1) replacement 												  
