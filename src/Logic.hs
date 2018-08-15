module Logic (

    ) where

import qualified Data.Char as CH

signedF :: Bool -> Bool
signedF  = not

signedT :: Bool -> Bool
signedT x = x

type Var = String
data Prop a = Atom a | Not (Prop a) 
                       | Imply (Prop a)  (Prop a) 
                       | And (Prop a) (Prop a)
                       | Or  (Prop a) (Prop a)


instance (Show a) => Show (Prop a) where
    show (Atom p) = show p
    show (Not prop) = "(-"++ show prop  ++")"
    show (Imply p1 p2) = "(" ++ show p1 ++ " => " ++ show p2 ++ ")"
    show (And p1 p2) = "(" ++ show p1 ++ " ^ " ++ show p2 ++ ")"
    show (Or p1 p2) = "(" ++ show p1 ++ " | " ++ show p2 ++ ")"

instance (Ord a) =>Eq (Prop a) where
    (Atom p) == (Atom q) = p == q
    (Not p)  == (Not q)  = p == q
    (Imply p q) == (Imply p1 q1) = (p == p1) && (q == q1)
    (And p q) == (And p1 q1) = (p == p1) && (q == q1)
    (Or p q) == (Or p1 q1) = (p == p1) || (q == q1)
    _ == _ = False 

pvalue :: (a -> Bool) -> Prop a -> Bool
pvalue vf (Atom p) = vf p
pvalue vf (Not p)  = not (pvalue vf p)
pvalue vf (Imply p q) = pvalue vf (Not p) || pvalue vf q
pvalue vf (And p q)   = pvalue vf q && pvalue vf p
pvalue vf (Or p q)    = pvalue vf q || pvalue vf p

{-
    Usage:

    Atom '4'

    And (Atom '4') (Atom '5')

    pvalue isDigit (And (Atom '4') (Atom '5'))

    pvalue isDigit $ Imply (And (Atom 'c') (Atom 'd')) (Not (Or (Not (Atom 'c')) (Not (Atom 'd'))))
-}



